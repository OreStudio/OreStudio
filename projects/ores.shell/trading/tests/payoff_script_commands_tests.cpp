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
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/trading/payoff_script_commands.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <random>
#include <sstream>
#include <string>
#include <vector>

using ores::shell::app::command_feedback;
using ores::shell::app::commands::payoff_script_commands;

namespace {

const std::string script_tags = "[shell][trading][script]";

/// A file of the given text in a fresh directory of its own.
class scratch_files final {
public:
    scratch_files()
        : dir_(std::filesystem::temp_directory_path() /
               ("ores_shell_script_" + std::to_string(std::random_device{}()))) {
        std::filesystem::create_directories(dir_);
    }
    ~scratch_files() {
        std::error_code ignored;
        std::filesystem::remove_all(dir_, ignored);
    }
    scratch_files(const scratch_files&) = delete;
    scratch_files& operator=(const scratch_files&) = delete;

    std::string write(const std::string& name, const std::string& text) const {
        const auto path = dir_ / name;
        std::ofstream(path, std::ios::binary) << text;
        return path.string();
    }
    std::string path(const std::string& name) const {
        return (dir_ / name).string();
    }

private:
    std::filesystem::path dir_;
};

struct run_result final {
    std::string output;
    bool failed = false;
};

template <typename Process>
run_result run(Process process, const std::vector<std::string>& args) {
    command_feedback::reset();
    std::ostringstream out;
    process(out, args);
    return {.output = out.str(), .failed = command_feedback::failed()};
}

const std::string payoff = "NUMBER unused;\n"
                           "REQUIRE Strike > 0;\n"
                           "value = PAY(Strike, Expiry, Expiry, PayCcy);\n";

}

TEST_CASE("script parse accepts a script and locates a syntax error", script_tags) {
    scratch_files files;
    const auto good = files.write("good.txt", payoff);
    const auto fine = run(payoff_script_commands::process_parse, {good});
    CHECK_FALSE(fine.failed);
    CHECK(fine.output.find("parses") != std::string::npos);

    const auto bad = files.write("bad.txt", "value = ;\n");
    const auto broken = run(payoff_script_commands::process_parse, {bad});
    CHECK(broken.failed);
    CHECK(broken.output.find(bad + ":1:") != std::string::npos);
    CHECK(broken.output.find("[SCR1001]") != std::string::npos);
}

TEST_CASE("script check without variables reports the script's own findings", script_tags) {
    scratch_files files;
    const auto path = files.write("payoff.txt", payoff);
    const auto result = run(payoff_script_commands::process_check, {path});
    // The unused declaration is a warning, and the inputs a trade would state
    // are not findings while no trade is given.
    CHECK_FALSE(result.failed);
    CHECK(result.output.find(path + ":1:1: warning:") != std::string::npos);
    CHECK(result.output.find("[SCR4001]") != std::string::npos);
    CHECK(result.output.find("SCR2001") == std::string::npos);
}

TEST_CASE("script check with variables reports errors and fails", script_tags) {
    scratch_files files;
    const auto path = files.write("payoff.txt", payoff);
    const auto variables = files.write("variables.tsv",
                                       "# a trade's variables\n"
                                       "Strike\tNumber\t-5\n"
                                       "Expiry\tEvent[]\t2027-01-01,2028-01-01\n");
    const auto result =
        run(payoff_script_commands::process_check, {path, "--variables", variables});
    CHECK(result.failed);
    CHECK(result.output.find(path + ":2:1: error:") != std::string::npos);
    CHECK(result.output.find("[SCR3001]") != std::string::npos);
    CHECK(result.output.find("[SCR2004]") != std::string::npos);
    CHECK(result.output.find("[SCR2001]") != std::string::npos);

    const auto bad = files.write("bad.tsv", "Strike\tNumber\n");
    const auto refused = run(payoff_script_commands::process_check, {path, "--variables", bad});
    CHECK(refused.failed);
    CHECK(refused.output.find(bad + ":1:") != std::string::npos);
}

TEST_CASE("script inputs lists one input per line and merges variants", script_tags) {
    scratch_files files;
    const auto first = files.write("a.txt",
                                   "REQUIRE Strike > 0\n  AND Strike < 10;\n"
                                   "value = PAY(Strike, Expiry, Expiry, PayCcy);\n");
    const auto second = files.write("b.txt", "value = PAY(Fixings[1], Expiry, Expiry, PayCcy);\n");
    const auto merged = run(payoff_script_commands::process_inputs, {first, second});
    REQUIRE_FALSE(merged.failed);
    // The rule is the REQUIRE's condition on one line, and the variants'
    // inputs are merged in the order the parser derives them.
    CHECK(merged.output == "Strike\tNumber\t0\tStrike > 0 AND Strike < 10\n"
                           "Expiry\tEvent\t0\t\n"
                           "PayCcy\tCurrency\t0\t\n"
                           "Fixings\tNumber\t1\t\n");

    const auto out = files.path("inputs.tsv");
    const auto written = run(payoff_script_commands::process_inputs, {second, "--out", out});
    REQUIRE_FALSE(written.failed);
    std::ifstream in(out);
    std::stringstream text;
    text << in.rdbuf();
    CHECK(text.str() == "Expiry\tEvent\t0\t\nPayCcy\tCurrency\t0\t\nFixings\tNumber\t1\t\n");
}

TEST_CASE("script print writes the normal form and a missing file fails", script_tags) {
    scratch_files files;
    const auto path = files.write("payoff.txt", "value=PAY(Strike,Expiry,Expiry,PayCcy);");
    const auto printed = run(payoff_script_commands::process_print, {path});
    CHECK_FALSE(printed.failed);
    CHECK(printed.output.find("PAY(Strike, Expiry, Expiry, PayCcy)") != std::string::npos);

    const auto missing = run(payoff_script_commands::process_print, {files.path("absent.txt")});
    CHECK(missing.failed);
    CHECK(missing.output.find("Cannot read") != std::string::npos);
}
