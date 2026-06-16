"""
run_sky_brightness.py

Batch-processes CR2 fisheye images through DiCaLum and outputs:
  output/sky_brightness_per_image.csv  — one row per image
  output/sky_brightness_measurements.csv — one row per site (p1/p2/p3 averaged),
     ready to drop into the ColterBay R pipeline unchanged.

Activate your venv and install dependencies first:
    source environment/bin/activate
    python -m pip install dicalum==4.0b7

Then run with the ▶ button in VS Code, or:
    python run_sky_brightness.py

Filename convention:  grte_<SITE>_dark_p<N>.CR2
"""

# ── CONFIGURATION ─────────────────────────────────────────────────────────────

RAW_DIR = "data/raw_images"   # folder containing .CR2 files
OUT_DIR = "output"             # folder for CSV outputs

# Camera and lens indices (must match DiCaLum's built-in lists):
#   cameras: 0=EOS6D  1=EOS60D  2=M100  3=M200  4=SonyA7S
#   lenses:  0=Sigma8mm  1=Sigma4.5mm  2=Samyang8mm  3=Samyang24mm
#            4=Samyang50mm  5=Meike6.5mm  6=NoCorrection
CAMERA_IDX = 0   # EOS6D
LENS_IDX   = 0   # Sigma8mm

# ─────────────────────────────────────────────────────────────────────────────

import numpy as np
import pandas as pd
from pathlib import Path

try:
    import dicalum
except ImportError:
    raise ImportError(
        "DiCaLum not installed.\n"
        "Activate your venv and run:  python -m pip install dicalum==4.0b7"
    )

try:
    import exifread
except ImportError:
    raise ImportError("exifread not installed.  Run:  python -m pip install exifread")


def read_exif(filepath: Path):
    """Read ISO, aperture, shutter from EXIF. Returns (iso, aperture, shutter) as floats."""
    with open(filepath, "rb") as f:
        tags = exifread.process_file(f)

    def parse(tag):
        val = str(tags.get(tag, "0"))
        if "/" in val:
            n, d = val.split("/")
            return float(n) / float(d)
        return float(val) if val else 0.0

    return parse("EXIF ISOSpeedRatings"), parse("EXIF FNumber"), parse("EXIF ExposureTime")

# Hide the Tk window DiCaLum opens at import — we don't need the GUI
dicalum.TopWin.withdraw()

# Set camera and lens, then build the vignetting correction matrix
dicalum.dclinst.camera = CAMERA_IDX
dicalum.dclinst.lens   = LENS_IDX
dicalum.setvig()
print(f"Camera : {dicalum.cameras[CAMERA_IDX]}")
print(f"Lens   : {dicalum.lenses[LENS_IDX]}")
print(f"Vignetting matrix set — {dicalum.dcldat.V.shape}\n")


def extract_site(filename_stem):
    """
    'grte_cb1_dark_p1' → 'CB1'
    Matches R pipeline logic: toupper(word(stem, 2, sep="_"))
    """
    parts = filename_stem.split("_")
    return parts[1].upper() if len(parts) >= 2 else filename_stem.upper()


def process_file(cr2_path: Path) -> dict | None:
    print(f"  Processing: {cr2_path.name} ...", end=" ", flush=True)

    # Read EXIF ourselves — DiCaLum's internal EXIF reader can silently fail
    # under a withdrawn Tk window, zeroing out the exposure values.
    iso, aperture, shutter = read_exif(cr2_path)

    # Sigma 8mm doesn't transmit aperture via EXIF (manual aperture ring).
    # Fall back to the lens calibration value stored in DiCaLum — same logic
    # as rawread()'s internal fallback.
    if aperture == 0:
        aperture = dicalum.LensList[LENS_IDX].aper

    if iso == 0 or shutter == 0:
        print(f"FAILED (EXIF missing — iso={iso} t={shutter})")
        return None

    ddat = dicalum.rawread(str(cr2_path))

    if isinstance(ddat, int) and ddat == -1:
        print("FAILED (rawread returned -1 — check camera/file compatibility)")
        return None

    try:
        # Apply DSU exposure correction manually using our EXIF values.
        # Same formula as dicalum.dsu() — epo = 6400 * f² / (ISO × t) —
        # but bypasses the broken StringVar path inside rawread.
        epo   = 6400 * aperture ** 2 / (iso * shutter)
        r_dsu = epo * ddat.R
        g_dsu = epo * ddat.G
        b_dsu = epo * ddat.B

        # Luminance-weighted DSU (ITU-R BT.709 coefficients, matches original script)
        lum_dsu = 0.2126 * r_dsu + 0.7152 * g_dsu + 0.0722 * b_dsu

        # Sky mask: ddat.M and the DSU arrays are both at demosaic resolution (1854×2784)
        sky = ddat.M

        mean_dsu    = float(np.mean(lum_dsu[sky]))
        median_dsu  = float(np.median(lum_dsu[sky]))
        sd_dsu      = float(np.std(lum_dsu[sky]))
        mean_dsu_g  = float(np.mean(g_dsu[sky]))   # green channel only

        print(f"done  (mean DSU = {mean_dsu:.4f})")

        return {
            "filename":    cr2_path.name,
            "site":        extract_site(cr2_path.stem),
            "mean_dsu":    mean_dsu,        # luminance-weighted, sky-masked
            "median_dsu":  median_dsu,
            "sd_dsu":      sd_dsu,
            "mean_dsu_g":  mean_dsu_g,      # green channel only (alternative)
            "iso":         iso,
            "aperture_f":  aperture,
            "exposure_s":  shutter,
        }

    except Exception as e:
        print(f"FAILED — {e}")
        return None


def main():
    raw_dir = Path(RAW_DIR)
    out_dir = Path(OUT_DIR)

    if not raw_dir.exists():
        raise FileNotFoundError(
            f"RAW_DIR not found: {raw_dir.resolve()}\n"
            "Update the RAW_DIR variable at the top of this script."
        )
    out_dir.mkdir(parents=True, exist_ok=True)

    cr2_files = sorted(raw_dir.glob("*.CR2")) + sorted(raw_dir.glob("*.cr2"))
    if not cr2_files:
        print(f"No CR2 files found in {raw_dir.resolve()}")
        return

    print(f"Found {len(cr2_files)} CR2 files\n")
    results = [r for f in cr2_files if (r := process_file(f)) is not None]

    if not results:
        print("No files processed successfully.")
        return

    # Per-image CSV
    df_img = pd.DataFrame(results)
    img_csv = out_dir / "sky_brightness_per_image.csv"
    df_img.to_csv(img_csv, index=False)

    # Per-site CSV (averages p1/p2/p3) — mean_brightness_site column used by R
    df_sites = (
        df_img
        .groupby("site", as_index=False)
        .agg(
            mean_brightness_site = ("mean_dsu",  "mean"),   # R pipeline joins on this
            mean_dsu_g_site      = ("mean_dsu_g","mean"),
            n_images             = ("filename",  "count"),
        )
    )
    site_csv = out_dir / "sky_brightness_measurements.csv"
    df_sites.to_csv(site_csv, index=False)

    print(f"\nDone! {len(results)} images | {df_sites.shape[0]} sites")
    print(f"  Per-image : {img_csv.resolve()}")
    print(f"  Per-site  : {site_csv.resolve()}\n")
    print(df_sites.to_string(index=False))


if __name__ == "__main__":
    main()
