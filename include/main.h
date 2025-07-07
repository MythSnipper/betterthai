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


void crash_out();

#endif