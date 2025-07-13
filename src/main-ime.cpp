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
//I use arch btw
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
    std::cout << "Starting IME(cpp file btw)\n";
    // Read file
    //std::string home = std::getenv("HOME");
    //std::ifstream dictfile(home + "/.emacs.d/betterthai-ime/data/dict.json");
    std::ifstream dictfile("/home/mythsnipper/.emacs.d/betterthai-ime/data/dict.json");
    if (!dictfile) {
        std::cerr << "No dictfile\n";
        return 1;
    }
    json data = json::parse(dictfile);
    // Load word frequencies
    std::ifstream freqfile("/home/mythsnipper/.emacs.d/betterthai-ime/data/word_freq.json");
    if (!freqfile) {
        std::cerr << "No freqfile\n";
        return 1;
    }
    json word_freq = json::parse(freqfile);

    // Extract all keys into a list
    std::vector<std::string> ipa_keys;
    for (auto it = data.begin(); it != data.end(); ++it) {
        ipa_keys.push_back(it.key());
    }

    std::string str;

    while (str != "exit") {
        std::getline(std::cin, str);
        str = run_espeak_ng(str);


        // Run fuzzy match
        auto matches = extract<std::string, std::vector<std::string>>(str, ipa_keys, 0.0);

        // Sort by descending score
        std::sort(matches.begin(), matches.end(),
                [](const auto& a, const auto& b) { return a.second > b.second; });

        // Trim to top 50
        if (matches.size() > 50) matches.resize(50);

        //thing to store info
        std::vector<std::tuple<std::string, int, double>> final_ranked;  // (thai, freq, score)

        for (const auto& [match, score] : matches) {
            const auto& ipas = data[match];
            bool has_valid = false;

            // First pass: filter Thai words to  display
            std::vector<std::pair<std::string, int>> all_thai;
            bool has_non_zero_freq = false;

            for (const auto& thai : ipas) {
                int freq = 0;
                if (word_freq.contains(thai) && word_freq[thai].is_number_integer()) {
                    freq = word_freq[thai];
                }
                all_thai.emplace_back(thai, freq);
                if (freq > 0) has_non_zero_freq = true;
            }

            // Filter out zero-frequency words *only if* there's at least one non-zero
            std::vector<std::pair<std::string, int>> filtered_thai;
            for (const auto& [thai, freq] : all_thai) {
                if (score >= 69.0 || freq > 0) {
                    if (has_non_zero_freq && freq == 0) continue;  // Skip 0-freq if others exist
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
                std::cout << "  → " << thai << " (freq: " << freq << ")  " << "hen" << "\n";
                final_ranked.emplace_back(thai, freq, score);
            } 
        }
        // Print summary of top 10 Thai words by order of appearance
            // Sort by score descending, then freq descending
        std::sort(final_ranked.begin(), final_ranked.end(),
            [](const auto& a, const auto& b) {
                if (std::get<2>(a) != std::get<2>(b))  // Compare score
                    return std::get<2>(a) > std::get<2>(b);
                else  // Tie-breaker: frequency
                    return std::get<1>(a) > std::get<1>(b);
        });

        std::cout << "\nTop 10 Thai words:\n";
        for (size_t i = 0; i < std::min<size_t>(10, final_ranked.size()); ++i) {
            const auto& [thai, freq, score] = final_ranked[i];
            std::cout << (i + 1) << ". " << thai << " (freq: " << freq << ", score: " << score << ")\n";
        }
        std::cout.flush();
    }

    return 0;
}
