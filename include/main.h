#ifndef MAIN_H
#define MAIN_H

#include <string>
#include <iostream>
#include <fstream>
#include <vector>

#include "rapidfuzz/fuzz.hpp"
#include "nlohmann/json.hpp"

using rapidfuzz::fuzz::ratio;
using json = nlohmann::json;

void jesus(const std::string& dictStr, double score,
           std::vector<std::string>& best_ipas,
           std::vector<double>& best_values);

#endif