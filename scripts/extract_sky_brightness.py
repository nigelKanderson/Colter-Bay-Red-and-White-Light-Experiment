import rawpy
import numpy as np
import pandas as pd
from pathlib import Path
import exifread

#=========================================================
# Paths
#=========================================================

PROJECT_DIR = Path(".")
RAW_DIR = PROJECT_DIR / "data" / "raw_images"
OUTPUT_DIR = PROJECT_DIR / "output"
OUTPUT_DIR.mkdir(exist_ok=True)

#=========================================================
# FUNCTIONS
#=========================================================

def get_exif_data(filepath):

    with open(filepath, 'rb') as f:
        tags = exifread.process_file(f)

    def safe_get(tag):
        return str(tags[tag]) if tag in tags else None
    
    return{
        "iso": safe_get("EXIF ISOSpeedRatings"),
        "exposure": safe_get("EXIF ExposureTime"),
        "aperture": safe_get("EXIF FNumber")
    }

def parse_fraction(frac_string):
    """Convert EXIF fractions like '1/60' to float."""

    if frac_string is None:
        return None
    
    if "/" in frac_string:
        num, den = frac_string.split("/")
        return float(num) / float(den)
    
    return float(frac_string)

def calculate_luminance(rgb):
    """Convert RGB image to luminance image."""

    luminance = (
        0.2126 * rgb[:, :, 0] +
        0.7152 * rgb[:, :, 1] +
        0.0722 * rgb[:, :, 2]
    )

    return luminance

def crop_center_region(image, fraction=0.5):
    """Crop central image region to avoid horizon artifacts"""

    h, w = image.shape

    start_h = int(h * (1 - fraction) / 2)
    end_h = int(h * (1 + fraction) / 2)

    start_w = int(w * (1 - fraction) / 2)
    end_w = int(w * (1 + fraction) / 2)

    return image[start_h:end_h, start_w:end_w]

def exposure_corrected_brightness(brightness, iso, exposure, aperture):
    """
    Normalize brightness across exposure settings.
    corrected = brightness * (f^2) / (ISO * exposure)
    """

    if iso is None or exposure is None or aperture is None:
        return np.nan
    
    return brightness * (aperture ** 2) / (iso * exposure)

#=========================================================
# PROCESS IMAGES
#=========================================================

results = []

files = sorted(RAW_DIR.glob("*.CR2"))

print(f"found {len(files)} RAW images")

for file in files:

    print(f"Processing: {file.name}")

    # Read raw image

    with rawpy.imread(str(file)) as raw:
        rgb = raw.postprocess(
            use_camera_wb=True,
            no_auto_bright=True,
            gamma=(1,1),
            output_bps=16

        )

        #Calculate Luminance

    luminance = calculate_luminance(rgb)

    #Normalize to 0-1
    luminance = luminance / np.max(luminance)

    #Crop Center Region

    center = crop_center_region(luminance, fraction=0.5)

    #Summary Statistics

    mean_brightness = np.mean(center)
    median_brightness = np.median(center)
    sd_brightness = np.std(center)

    #Exif Data

    exif = get_exif_data(file)

    iso = parse_fraction(exif["iso"])
    exposure = parse_fraction(exif['exposure'])
    aperture = parse_fraction(exif["aperture"])

    #Exposure-corrected brightness

    corrected_brightness = exposure_corrected_brightness(
        mean_brightness,
        iso,
        exposure,
        aperture
    )

    # Store Results

    results.append({
        "filename": file.name,
        "mean_brightness": mean_brightness,
        "median_brightness": median_brightness,
        "sd_brightness": sd_brightness,
        "corrected_brightness": corrected_brightness,
        "iso": iso,
        "exposure_seconds": exposure,
        "aperture_f": aperture
    })

# Export CSV

results_df = pd.DataFrame(results)

output_file = OUTPUT_DIR / "sky_brightness_measurements.csv"

results_df.to_csv(output_file, index=False)

print("\nDone!")
print(f"Results saved to: {output_file}")
print(results_df.head())