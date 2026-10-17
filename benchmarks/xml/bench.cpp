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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
// One XML benchmark, three modes, so the cost of the document model can be
// separated from the cost of scanning:
//
//   scan  reads the events and keeps nothing
//   tape  builds the transient document model
//   pugi  builds a pugixml DOM, the baseline the model is judged against

#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/scanner.hpp"
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <pugixml.hpp>
#include <string>

using namespace ores::ore::xml;

namespace {

auto slurp(const char* path, std::string& text) -> bool {
    std::ifstream file(path, std::ios::binary);
    if (!file)
        return false;
    text.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
    return !text.empty();
}

auto scan(const std::string& text) -> std::size_t {
    reader input(text);
    std::size_t seen = 0;
    for (;;) {
        const auto ev = input.next();
        if (ev.kind == event_type::end_of_document)
            return seen;
        if (ev.kind == event_type::error)
            return 0;
        seen += ev.name.size() + ev.value.size() + 1;
    }
}

auto tape(const std::string& text) -> std::size_t {
    const auto parsed = document::read(text);
    if (!parsed)
        return 0;
    return parsed->tokens().size();
}

// Only the load is measured. Walking the tree afterwards would add work the
// baseline does not need and would flatter the codec by comparison.
auto dom(const std::string& text) -> std::size_t {
    pugi::xml_document tree;
    return tree.load_buffer(text.data(), text.size()) ? 1 : 0;
}

}

int main(int, char** argv) {
    if (argv[1] == nullptr || argv[2] == nullptr || argv[3] == nullptr) {
        std::printf("usage: bench <scan|tape|pugi> <document> <repeats>\n");
        return 1;
    }
    const std::string mode(argv[1]);
    std::string text;
    if (!slurp(argv[2], text)) {
        std::printf("could not read %s\n", argv[2]);
        return 1;
    }
    const int repeats = std::atoi(argv[3]);
    if (repeats <= 0) {
        std::printf("repeats must be a positive number, not %s\n", argv[3]);
        return 1;
    }

    std::size_t (*run)(const std::string&) = nullptr;
    if (mode == "scan")
        run = scan;
    else if (mode == "tape")
        run = tape;
    else if (mode == "pugi")
        run = dom;
    else {
        std::printf("unknown mode %s\n", mode.c_str());
        return 1;
    }

    std::size_t totals = 0;
    const auto from = std::chrono::steady_clock::now();
    for (int i = 0; i < repeats; ++i)
        totals += run(text);
    const auto elapsed =
        std::chrono::duration<double, std::nano>(std::chrono::steady_clock::now() - from).count();

    if (totals == 0) {
        std::printf("the document did not parse\n");
        return 1;
    }
    std::printf("ns_per_parse %.1f checksum %zu\n", elapsed / repeats, totals);
    return 0;
}
