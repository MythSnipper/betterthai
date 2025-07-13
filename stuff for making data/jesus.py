import requests
from bs4 import BeautifulSoup
import json
import os
import pythainlp
from pythainlp.util import remove_tone_ipa, remove_zw
from pythainlp.transliterate import transliterate
from concurrent.futures import ThreadPoolExecutor, as_completed

MAX_WORKERS = 20  # Adjust to be polite to Wiktionary

def fetch_ipa_from_wiktionary(word):
    url = f"https://en.wiktionary.org/wiki/{word}"
    try:
        r = requests.get(url, timeout=10)
        while r.status_code == 429:
            print("RETRYINGGGG")
            r = requests.get(url, timeout=10)
        if r.status_code != 200:
            
            print(f"❌ HTTP {r.status_code}: Could not fetch '{word}' from Wiktionary.")
            return []
    except requests.exceptions.RequestException as e:
        print(f"❌ Network error while fetching '{word}': {e}")
        return []

    soup = BeautifulSoup(r.text, "html.parser")
    ipa_tags = soup.select("span.IPA")
    ipas = [tag.text.strip() for tag in ipa_tags if tag.text.startswith("/")]
    return ipas

def fallback_with_thainlp(word):
    ipa = transliterate(word, engine="ipa")
    if ipa:
        print(f"🔄 Fallback using ThaiNLP for '{word}': {ipa}")
    else:
        print(f"⚠️  ThaiNLP failed to transliterate '{word}'")
    return [ipa] if ipa else []

def clean_and_strip_tone(ipa):
    cleane = remove_zw(ipa)
    cipa = remove_tone_ipa(cleane)
    for tone_mark in ["˥", "˦", "˧", "˨", "˩"]:
        cipa = cipa.replace(tone_mark, "")
    return cipa

def process_word(word):
    print(f"🔍 Fetching IPA for: {word}")
    ipas = fetch_ipa_from_wiktionary(word)

    if not ipas:
        ipas = fallback_with_thainlp(word)

    if not ipas:
        print(f"⚠️  Skipping '{word}' — no IPA available.")
        return None

    results = []
    for ipa in ipas:
        clean_ipa = ipa.strip("/ ")
        if clean_ipa:
            stripped_ipa = clean_and_strip_tone(clean_ipa)
            results.append((stripped_ipa, word))
    return results

def build_ipa_dict_concurrent(word_list):
    ipa_dict = {}

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = {executor.submit(process_word, word): word for word in word_list}
        for future in as_completed(futures):
            result = future.result()
            if result:
                for ipa, word in result:
                    ipa_dict.setdefault(ipa, []).append(word)

    return ipa_dict

def load_words_from_file(filepath):
    with open(filepath, "r", encoding="utf-8") as f:
        return [line.strip() for line in f if line.strip()]

if __name__ == "__main__":
    input_path = "words_th.txt"
    output_path = "dict.json"

    word_list = load_words_from_file(input_path)
    ipa_dict = build_ipa_dict_concurrent(word_list)

    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(ipa_dict, f, ensure_ascii=False, indent=2)

    print("✅ IPA dictionary built and saved to:", output_path)