/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.utility/string/short_code_generator.hpp"

#include <cctype>
#include <vector>

namespace ores::utility::string {

namespace {

bool ends_with_ci(const std::string& s, const std::string& suffix) {
    if (suffix.size() > s.size()) return false;
    const auto start = s.size() - suffix.size();
    // Must be preceded by a space (or be the entire string).
    if (start > 0 && s[start - 1] != ' ') return false;
    for (std::size_t i = 0; i < suffix.size(); ++i) {
        if (std::toupper(static_cast<unsigned char>(s[start + i])) !=
            std::toupper(static_cast<unsigned char>(suffix[i])))
            return false;
    }
    return true;
}

void strip_suffix(std::string& result, const std::string& suffix) {
    if (ends_with_ci(result, suffix)) {
        result.resize(result.size() - suffix.size());
        while (!result.empty() && result.back() == ' ')
            result.pop_back();
    }
}

}

std::string strip_corporate_suffixes(const std::string& name) {
    std::string result = name;

    // Trim trailing spaces.
    while (!result.empty() && result.back() == ' ')
        result.pop_back();

    // Dotted abbreviations (longer patterns first).
    for (const auto& s : {"S.C.A.", "S.R.L.", "S.P.A.", "S.A.",
             "B.V.", "N.V.", "K.K.", "S.E."})
        strip_suffix(result, s);

    // Word suffixes.
    for (const auto& s : {"AND CO.", "& CO.", "CO.",
             "PLC", "LLC", "LLP", "LTD", "INC", "CORP",
             "AG", "NV", "AB", "ASA", "GMBH", "MBH",
             "BV", "BHD", "SRL", "SPA", "SARL", "KG",
             "OYJ", "TBK", "LP", "SE", "SA", "AS"})
        strip_suffix(result, s);

    return result;
}

std::string generate_short_code(const std::string& name) {
    // Step 1: strip corporate suffixes.
    auto input = strip_corporate_suffixes(name);

    // Step 2: uppercase, remove non-alpha (keep spaces), collapse whitespace.
    std::string clean;
    bool prev_space = false;
    for (char c : input) {
        if (std::isalpha(static_cast<unsigned char>(c))) {
            clean += static_cast<char>(
                std::toupper(static_cast<unsigned char>(c)));
            prev_space = false;
        } else if (c == ' ' && !clean.empty() && !prev_space) {
            clean += ' ';
            prev_space = true;
        }
    }
    while (!clean.empty() && clean.back() == ' ')
        clean.pop_back();

    // Step 3: empty check.
    if (clean.empty())
        return "UNKNWN";

    // Step 4: split into words.
    std::vector<std::string> words;
    std::string word;
    for (char c : clean) {
        if (c == ' ') {
            if (!word.empty()) {
                words.push_back(std::move(word));
                word.clear();
            }
        } else {
            word += c;
        }
    }
    if (!word.empty())
        words.push_back(std::move(word));

    std::string result;
    if (words.size() == 1) {
        // Single word: first letter + consonants (skip vowels), up to 6.
        const auto& w = words[0];
        std::string consonants;
        for (char c : w) {
            if (c != 'A' && c != 'E' && c != 'I' && c != 'O' && c != 'U')
                consonants += c;
        }
        result = w.substr(0, 1);
        if (consonants.size() > 1)
            result += consonants.substr(1);
        if (result.size() > 6)
            result.resize(6);
    } else if (words.size() == 2) {
        // Two words: 3+3.
        result = words[0].substr(0, 3) + words[1].substr(0, 3);
    } else {
        // Three+ words: 2+2+2.
        result = words[0].substr(0, 2) + words[1].substr(0, 2)
            + words[2].substr(0, 2);
    }

    // Pad to minimum 3 chars.
    while (result.size() < 3)
        result += 'X';

    return result;
}

std::string generate_unique_short_code(const std::string& name,
    std::unordered_set<std::string>& used_codes) {

    const auto base = generate_short_code(name);

    auto code = base;
    int suffix = 2;
    while (used_codes.contains(code))
        code = base + std::to_string(suffix++);
    used_codes.insert(code);
    return code;
}

}
