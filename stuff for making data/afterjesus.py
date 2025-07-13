import json
from collections import defaultdict

input_path = "dict.json"
output_path = "dictd.json"

with open(input_path, "r", encoding="utf-8") as f:
    data = json.load(f)

# Use defaultdict to merge values for keys that collapse after dot removal
merged_data = defaultdict(list)

for k, v in data.items():
    clean_key = k.replace('.', '')
    for annoying in ["ʰ","◌̚","◌͡◌","◌͡"]:
        clean_key = clean_key.replace (annoying,"")
    clean_key1 = clean_key.replace("t̚","t")
    clean_key2 = clean_key1.replace("k̚","k")
    merged_data[clean_key2].extend(v)

# Deduplicate each list
deduped_data = {k: list(set(v)) for k, v in merged_data.items()}

# Save the cleaned dictionary
with open(output_path, "w", encoding="utf-8") as f:
    json.dump(deduped_data, f, ensure_ascii=False, indent=2)

print(f"✅ Dots removed, collisions merged, and duplicates removed. Saved to: {output_path}")
