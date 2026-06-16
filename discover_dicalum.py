"""
discover_dicalum.py

Run this ONCE after installing DiCaLum to reveal the API and output structure.
Share the printed output so the batch script can be written correctly.

From your project folder with the venv active:
    python discover_dicalum.py data/raw_images/grte_amla1_dark_p1.CR2
"""

import sys
import json
import inspect

try:
    import dicalum
except ImportError:
    sys.exit(
        "DiCaLum not installed.\n"
        "Activate your venv and run:\n"
        "    python -m pip install dicalum==4.0b7"
    )

print("=" * 60)
print("DICALUM PACKAGE CONTENTS")
print("=" * 60)
print(f"Location: {dicalum.__file__}")
print(f"Version:  {getattr(dicalum, '__version__', 'unknown')}")
print()

for name in sorted(dir(dicalum)):
    if name.startswith("__"):
        continue
    obj = getattr(dicalum, name)
    kind = ("function" if inspect.isfunction(obj) else
            "class"    if inspect.isclass(obj)    else
            "module"   if inspect.ismodule(obj)   else type(obj).__name__)
    print(f"  {kind:10s}  {name}")
    if inspect.isfunction(obj):
        try:
            print(f"             sig: {name}{inspect.signature(obj)}")
        except Exception:
            pass

print()

if len(sys.argv) < 2:
    print("No CR2 path given — skipping live test.")
    print("Usage:  python discover_dicalum.py data/raw_images/grte_amla1_dark_p1.CR2")
    sys.exit(0)

cr2_path = sys.argv[1]
print("=" * 60)
print(f"RUNNING ON: {cr2_path}")
print("=" * 60)

params = {
    "MODE":      "FR",
    "TIF FILE":  "N",
    "RAW FILE":  cr2_path,
    "MAXDSU":    100,
    "MAXPLOT":   41,
    "CAMERA":    "EOS6D",
    "LENS":      "Sigma8mm",
    "ISO":       0,
    "APERTURE":  0,
    "SHUTTER":   0,
    "TITLE":     "test",
    "OBSERVER":  "",
    "LOCATION":  "",
    "LONGITUDE": 0,
    "LATITUDE":  0,
    "DATE":      "N",
    "TIME":      "N",
}

result = None
for fn_name in ["run", "process", "main", "dicalum", "DiCaLum", "calculate"]:
    fn = getattr(dicalum, fn_name, None)
    if fn is None:
        continue
    print(f"Trying dicalum.{fn_name}(params) ...", end=" ", flush=True)
    try:
        result = fn(params)
        print("SUCCESS")
        break
    except TypeError:
        try:
            result = fn(json.dumps(params))
            print("SUCCESS (json string)")
            break
        except Exception as e:
            print(f"failed: {e}")
    except Exception as e:
        print(f"failed: {e}")

print()
if result is not None:
    print("=" * 60)
    print("OUTPUT:")
    print("=" * 60)
    try:
        print(json.dumps(result, indent=2, default=str))
    except Exception:
        print(repr(result))
else:
    print("Could not call DiCaLum — paste the PACKAGE CONTENTS above.")
