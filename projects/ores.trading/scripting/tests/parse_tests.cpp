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
#include "library_files.hpp"
#include "ores.trading.scripting/parse.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <optional>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace {

using namespace ores::trading::scripting;
using ores::trading::scripting::tests::file_text;
using ores::trading::scripting::tests::project_root;
namespace fs = std::filesystem;

const std::string parse_tags = "[trading][scripting][parse]";


/// One script of a library file: where it is, its name and purpose, its code.
struct library_script {
    std::string file;
    std::string name;
    std::string purpose;
    std::string code;
};

/// Every script variant of every script library ORE's examples ship.
std::vector<library_script> library_scripts(std::size_t* code_blocks = nullptr) {
    std::vector<library_script> out;
    const auto examples = project_root() / "external/ore/examples";
    const std::regex name(R"re(<Name>(\w+)</Name>)re");
    const std::regex variant(
        R"re(<Script(?:\s+purpose="(\w+)")?[^>]*>\s*<Code>\s*<!\[CDATA\[([\s\S]*?)\]\]>\s*</Code>)re");
    for (const auto& entry : fs::recursive_directory_iterator(examples)) {
        const auto file = entry.path().filename().string();
        if (!entry.is_regular_file() || !file.starts_with("scriptlibrary") ||
            entry.path().extension() != ".xml")
            continue;
        const auto text = file_text(entry.path());
        const auto relative = fs::relative(entry.path(), examples).generic_string();
        if (code_blocks)
            for (auto at = text.find("<Code>"); at != std::string::npos;
                 at = text.find("<Code>", at + 1))
                ++*code_blocks;
        for (auto v = std::sregex_iterator(text.begin(), text.end(), variant);
             v != std::sregex_iterator();
             ++v) {
            // A variant belongs to the script whose name precedes it last.
            std::string script_name;
            const auto before = text.substr(0, static_cast<std::size_t>(v->position()));
            for (auto n = std::sregex_iterator(before.begin(), before.end(), name);
                 n != std::sregex_iterator();
                 ++n)
                script_name = (*n)[1].str();
            out.push_back({.file = relative,
                           .name = script_name,
                           .purpose = (*v)[1].matched ? (*v)[1].str() : "default",
                           .code = (*v)[2].str()});
        }
    }
    return out;
}

}

TEST_CASE("every script ORE's examples ship parses, but the one ORE cannot", parse_tags) {
    std::size_t code_blocks = 0;
    const auto scripts = library_scripts(&code_blocks);
    // Every code block of every library is read, so none escapes the test.
    REQUIRE(code_blocks > 0u);
    REQUIRE(scripts.size() == code_blocks);

    // ORE's WorstOfBasketSwap states a statement with no semicolon, and ORE cannot
    // run it; the script library's corrections fix it in the store.
    const std::set<std::string> known = {"WorstOfBasketSwap/default"};
    std::set<std::string> failed;
    for (const auto& s : scripts) {
        const auto result = parse(s.code);
        if (result.parsed())
            continue;
        INFO(s.file << " " << s.name << "/" << s.purpose);
        REQUIRE(result.diagnostics.size() == 1u);
        CHECK(result.diagnostics[0].code == "SCR1001");
        CHECK(result.diagnostics[0].start.has_value());
        if (s.name == "WorstOfBasketSwap") {
            // ORE's own run reports "expected <InstructionSequence> in L17:30".
            const auto& d = result.diagnostics[0];
            CHECK(d.message == "expected <InstructionSequence>");
            CHECK(d.start->line == 17u);
            CHECK(d.start->column == std::optional<std::size_t>{30});
        }
        failed.insert(s.name + "/" + s.purpose);
    }
    CHECK(failed == known);
}

TEST_CASE("a parsed script reads back the same once written out", parse_tags) {
    for (const auto& s : library_scripts()) {
        const auto first = parse(s.code);
        if (!first.parsed())
            continue;
        INFO(s.file << " " << s.name << "/" << s.purpose);
        const auto written = ast_to_script(first.tree);
        const auto second = parse(written);
        REQUIRE(second.parsed());
        CHECK(ast_to_script(second.tree) == written);
        CHECK(print_ast(second.tree) == print_ast(first.tree));
    }
}

TEST_CASE("a random tree written out parses to the same tree", parse_tags) {
    for (std::size_t seed = 1; seed <= 200; ++seed) {
        INFO("seed " << seed);
        const auto tree = generate_random_ast(6, 4, seed);
        const auto written = ast_to_script(tree);
        const auto result = parse(written);
        INFO(written);
        REQUIRE(result.parsed());
        CHECK(print_ast(result.tree) == print_ast(tree));
    }
}

TEST_CASE("a syntax error is located by line and code point column", parse_tags) {
    // ORE reports a missing term where it begins: the space after the '=' on
    // line 3. Spirit widens the tab before it to four columns and reports column
    // 8; counted in code points, a tab as one, the column is 5.
    const std::string script = "NUMBER x;\nx = 1;\n\tx = + ;\n";
    const auto result = parse(script);
    REQUIRE_FALSE(result.parsed());
    REQUIRE(result.diagnostics.size() == 1u);
    const auto& d = result.diagnostics[0];
    CHECK(d.severity == severity::error);
    CHECK(d.code == "SCR1001");
    CHECK(d.message == "expected <Term>");
    REQUIRE(d.start.has_value());
    CHECK(d.start->line == 3u);
    REQUIRE(d.start->column.has_value());
    CHECK(*d.start->column == 5u);
}

TEST_CASE("a script with Windows line endings is located as one without", parse_tags) {
    const auto result = parse("NUMBER x;\r\nx = 1;\r\n\tx = + ;\r\n");
    REQUIRE_FALSE(result.parsed());
    REQUIRE(result.diagnostics.size() == 1u);
    REQUIRE(result.diagnostics[0].start.has_value());
    CHECK(result.diagnostics[0].start->line == 3u);
    CHECK(result.diagnostics[0].start->column == std::optional<std::size_t>{5});
}

TEST_CASE("a diagnostic prints as a compiler reports one", parse_tags) {
    diagnostic d;
    d.code = "SCR1001";
    d.message = "expected <InstructionSequence>";
    d.start = position{.line = 17, .column = 30};
    CHECK(to_compiler_format(d, "worst_of.ore") ==
          "worst_of.ore:17:30: error: expected <InstructionSequence> [SCR1001]");
    d.start->column.reset();
    d.severity = severity::warning;
    CHECK(to_compiler_format(d, "worst_of.ore") ==
          "worst_of.ore:17: warning: expected <InstructionSequence> [SCR1001]");
}

TEST_CASE("every code a finding states has its meaning stated once", parse_tags) {
    std::set<std::string> seen;
    for (const auto& c : diagnostic_codes()) {
        CHECK(seen.insert(std::string(c.code)).second);
        CHECK_FALSE(c.title.empty());
        CHECK_FALSE(c.explanation.empty());
    }
    CHECK(code_of("SCR1001").has_value());
    CHECK_FALSE(code_of("SCR9999").has_value());
}
