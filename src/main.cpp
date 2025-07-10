#include "../include/main.h"

template <typename Sentence1,
          typename Iterable, typename Sentence2 = typename Iterable::value_type>
std::vector<std::pair<Sentence2, double>>
extract(const Sentence1& query, const Iterable& choices, const double score_cutoff = 0.0)
{
    std::vector<std::pair<Sentence2, double>> results;
    rapidfuzz::fuzz::CachedRatio<typename Sentence1::value_type> scorer(query);

    for (const auto& choice : choices) {
        double score = scorer.similarity(choice, score_cutoff);
        if (score >= score_cutoff) {
            results.emplace_back(choice, score);
        }
    }

    return results;
}

bool is_arch_linux() {
    std::ifstream os_release("/etc/os-release");
    std::string line;
    while (std::getline(os_release, line)) {
        if (line.find("ID=arch") != std::string::npos) {
            return true;
        }
    }
    return false;
}

std::string run_espeak_ng(std::string text){
    if (!is_arch_linux()) {
        std::cout << "Not running on Arch Linux. Imagine not using Arch Linux :skull:\n";
        return "";
    }

    std::string command = "espeak-ng --ipa=3 -qx \"" + text + "\" 2>/dev/null";
    std::string result;
    char buffer[128];

    std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(command.c_str(), "r"), pclose);
    if (!pipe) {
        std::cerr << "Failed to run espeak-ng command\n";
        return "";
    }

    while (fgets(buffer, sizeof(buffer), pipe.get()) != nullptr) {
        result += buffer;
    }
    std::cout << "converted IPA: " << result << "\n";
    return result;
}

int main(int argc, char* argv[]) {
    std::cout << "testing betterthai:\n";

    // Read file
    std::ifstream dictfile("data/dict.json");
    if (!dictfile) {
        std::cout << "failed opening dictionary\n";
        return 1;
    }

    std::cout << "parsing json...\n";
    json data = json::parse(dictfile);
    std::cout << "done\n";

    // Load word frequencies
    std::ifstream freqfile("data/word_freq.json");
    if (!freqfile) {
        std::cout << "failed opening word_freq.json\n";
    return 1;
    }
json word_freq = json::parse(freqfile);

    // Extract all keys into a list
    std::vector<std::string> ipa_keys;
    for (auto it = data.begin(); it != data.end(); ++it) {
        ipa_keys.push_back(it.key());
    }

    std::string str;
    std::cout << "string: ";
    std::getline(std::cin, str);
    str = run_espeak_ng(str);

    while (str != "exit") {
        // Run fuzzy match
        auto matches = extract<std::string, std::vector<std::string>>(str, ipa_keys, 0.0);

        // Sort by descending score
        std::sort(matches.begin(), matches.end(),
                [](const auto& a, const auto& b) { return a.second > b.second; });

        // Trim to top 20
        if (matches.size() > 30) matches.resize(30);

for (const auto& [match, score] : matches) {
    const auto& ipas = data[match];
    bool has_valid = false;

    // First pass: filter Thai words to display
    std::vector<std::pair<std::string, int>> filtered_thai;
    for (const auto& thai : ipas) {
        int freq = 0;
        if (word_freq.contains(thai) && word_freq[thai].is_number_integer()) {
            freq = word_freq[thai];
        }

        if (score >= 69.0 || freq > 0) {
            filtered_thai.emplace_back(thai, freq);
            has_valid = true;
            }
        }

    // Only display matches with at least one valid Thai word
    if (!has_valid) continue;

    // Sort Thai words by descending frequency
    std::sort(filtered_thai.begin(), filtered_thai.end(),
        [](const auto& a, const auto& b) { return a.second > b.second; });

    std::cout << match << " (" << score << "):\n";
    for (const auto& [thai, freq] : filtered_thai) {
        std::cout << "  → " << thai << " (freq: " << freq << ")\n";
    }
}

        std::cout << "\nstring: ";
        std::getline(std::cin, str);
        str = run_espeak_ng(str);
    }

    return 0;
}
