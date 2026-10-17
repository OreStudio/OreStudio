/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
// One XML benchmark, three modes, so the cost of the document model can be
// separated from the cost of scanning:
//
//   scan  reads the events and keeps nothing
//   tape  builds the transient document model
//   pugi  builds a pugixml DOM, the baseline the model is judged against

#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/scanner.hpp"

#include <pugixml.hpp>

#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <string>

using namespace ores::ore::xml;

namespace {

auto slurp(const char* path) -> std::string {
    std::ifstream file(path, std::ios::binary);
    return std::string{std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>()};
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

auto dom(const std::string& text) -> std::size_t {
    pugi::xml_document tree;
    tree.load_buffer(text.data(), text.size());
    std::size_t nodes = 0;
    for (auto it = tree.begin(); it != tree.end(); ++it)
        ++nodes;
    return nodes;
}

}

int main(int, char** argv) {
    if (argv[1] == nullptr || argv[2] == nullptr || argv[3] == nullptr) {
        std::printf("usage: bench <scan|tape|pugi> <document> <repeats>\n");
        return 1;
    }
    const std::string mode(argv[1]);
    const std::string text = slurp(argv[2]);
    const int repeats = std::atoi(argv[3]);

    std::size_t totals = 0;
    const auto from = std::chrono::steady_clock::now();
    for (int i = 0; i < repeats; ++i) {
        if (mode == "scan")
            totals += scan(text);
        else if (mode == "tape")
            totals += tape(text);
        else if (mode == "pugi")
            totals += dom(text);
        else {
            std::printf("unknown mode %s\n", mode.c_str());
            return 1;
        }
    }
    const auto elapsed =
        std::chrono::duration<double, std::nano>(std::chrono::steady_clock::now() - from).count();

    if (totals == 0) {
        std::printf("the document did not parse\n");
        return 1;
    }
    std::printf("ns_per_parse %.1f checksum %zu\n", elapsed / repeats, totals);
    return 0;
}
