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
#include "ores.shell/app/commands/trading/payoff_script_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.trading.scripting/check.hpp"
#include "ores.trading.scripting/parse.hpp"
#include <cli/cli.h>
#include <fstream>
#include <optional>
#include <sstream>
#include <string_view>
#include <utility>

namespace ores::shell::app::commands {

namespace {

namespace scripting = ores::trading::scripting;

std::optional<std::string> read_script_file(const std::string& path) {
    std::ifstream in(path, std::ios::binary);
    if (!in)
        return std::nullopt;
    std::ostringstream text;
    text << in.rdbuf();
    return text.str();
}

/// A script that read and parsed, or nothing once the failure is reported.
struct parsed_script final {
    std::string text;
    scripting::parse_result result;
};

std::optional<parsed_script> parse_file(std::ostream& out, const std::string& path) {
    auto text = read_script_file(path);
    if (!text) {
        fail(out) << "Cannot read " << path << std::endl;
        return std::nullopt;
    }
    auto result = scripting::parse(*text);
    if (!result.parsed()) {
        for (const auto& d : result.diagnostics)
            out << scripting::to_compiler_format(d, path) << '\n';
        fail(out) << path << " does not parse" << std::endl;
        return std::nullopt;
    }
    return parsed_script{.text = std::move(*text), .result = std::move(result)};
}

/// The single positional a command takes, or nothing once the usage is shown.
std::optional<std::string>
one_file(std::ostream& out, const std::vector<std::string>& positionals, std::string_view usage) {
    if (positionals.size() != 1) {
        fail(out) << "Usage: " << usage << std::endl;
        return std::nullopt;
    }
    return positionals.front();
}

std::vector<std::string> split(std::string_view text, char separator) {
    std::vector<std::string> out;
    std::size_t start = 0;
    while (true) {
        const auto end = text.find(separator, start);
        out.emplace_back(text.substr(start, end == std::string_view::npos ? end : end - start));
        if (end == std::string_view::npos)
            return out;
        start = end + 1;
    }
}

std::optional<std::vector<scripting::script_variable>> read_variables(std::ostream& out,
                                                                      const std::string& path) {
    std::ifstream in(path);
    if (!in) {
        fail(out) << "Cannot read " << path << std::endl;
        return std::nullopt;
    }
    std::vector<scripting::script_variable> variables;
    std::string line;
    std::size_t number = 0;
    while (std::getline(in, line)) {
        ++number;
        if (!line.empty() && line.back() == '\r')
            line.pop_back();
        if (line.empty() || line.front() == '#')
            continue;
        const auto fields = split(line, '\t');
        if (fields.size() != 3) {
            fail(out) << path << ":" << number << ": expected name<TAB>type<TAB>values, found "
                      << fields.size() << " field(s)" << std::endl;
            return std::nullopt;
        }
        scripting::script_variable v{.name = fields[0], .type = fields[1]};
        if (v.type.ends_with("[]")) {
            v.type.resize(v.type.size() - 2);
            v.is_array = true;
        }
        if (v.name.empty() || v.type.empty()) {
            fail(out) << path << ":" << number << ": a variable needs a name and a type"
                      << std::endl;
            return std::nullopt;
        }
        if (!fields[2].empty())
            v.values = split(fields[2], ',');
        variables.push_back(std::move(v));
    }
    return variables;
}

std::string escaped(std::string_view text) {
    std::string out;
    for (const char c : text) {
        switch (c) {
            case '\\':
                out += "\\\\";
                break;
            case '\t':
                out += "\\t";
                break;
            case '\r':
                out += "\\r";
                break;
            case '\n':
                out += "\\n";
                break;
            default:
                out += c;
        }
    }
    return out;
}

/*
 * A finding about a trade's variables (SCR2xxx) or the values it states
 * (SCR3xxx), which means nothing while no trade is given. Every other finding
 * is the script's own, so a new code is reported unless it is a trade's.
 */
bool about_a_trade(const scripting::diagnostic& d) {
    return d.code.starts_with("SCR2") || d.code.starts_with("SCR3");
}

}

void payoff_script_commands::process_parse(std::ostream& out,
                                           const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    const auto path = one_file(out, parsed->positionals, "script parse <file>");
    if (!path)
        return;
    if (parse_file(out, *path))
        out << "✓ " << *path << " parses" << std::endl;
}

void payoff_script_commands::process_check(std::ostream& out,
                                           const std::vector<std::string>& args) {
    auto parsed =
        parse_args(args, {{.name = "variables", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    const auto path =
        one_file(out, parsed->positionals, "script check <file> [--variables <file>]");
    if (!path)
        return;
    const auto script = parse_file(out, *path);
    if (!script)
        return;

    const auto& variables_path = parsed->flag("variables");
    std::vector<scripting::script_variable> variables;
    if (!variables_path.empty()) {
        auto read = read_variables(out, variables_path);
        if (!read)
            return;
        variables = std::move(*read);
    }

    const auto inputs = scripting::inputs_of(script->result.tree, script->text);
    auto findings = scripting::check(script->result.tree, script->text, inputs, variables);
    if (variables_path.empty())
        std::erase_if(findings, about_a_trade);

    std::size_t errors = 0, warnings = 0;
    for (const auto& d : findings) {
        out << scripting::to_compiler_format(d, *path) << '\n';
        ++(d.severity == scripting::severity::error ? errors : warnings);
    }
    if (errors != 0)
        fail(out) << errors << " error(s) in " << *path << std::endl;
    else
        out << "✓ " << *path << ": " << warnings << " warning(s)" << std::endl;
}

void payoff_script_commands::process_inputs(std::ostream& out,
                                            const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {{.name = "out", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.empty()) {
        fail(out) << "Usage: script inputs <file> [<file>...] [--out <path>]" << std::endl;
        return;
    }
    std::vector<std::vector<scripting::script_input>> variants;
    for (const auto& path : parsed->positionals) {
        const auto script = parse_file(out, path);
        if (!script)
            return;
        variants.push_back(scripting::inputs_of(script->result.tree, script->text));
    }
    const auto inputs = variants.size() == 1 ? variants.front() : scripting::merge_inputs(variants);

    std::ostringstream table;
    for (const auto& in : inputs)
        table << escaped(in.name) << '\t' << escaped(in.type) << '\t' << (in.is_array ? 1 : 0)
              << '\t' << escaped(in.rule.value_or("")) << '\n';

    const auto& out_path = parsed->flag("out");
    if (out_path.empty() || out_path == "-") {
        out << table.str();
        return;
    }
    std::ofstream file(out_path, std::ios::binary);
    file << table.str();
    file.flush();
    if (!file) {
        fail(out) << "Cannot write " << out_path << std::endl;
        return;
    }
    out << "✓ Wrote " << inputs.size() << " input(s) to " << out_path << std::endl;
}

void payoff_script_commands::process_print(std::ostream& out,
                                           const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    const auto path = one_file(out, parsed->positionals, "script print <file>");
    if (!path)
        return;
    if (const auto script = parse_file(out, *path))
        out << scripting::ast_to_script(script->result.tree) << std::endl;
}

void payoff_script_commands::register_commands(cli::Menu& root_menu,
                                               ores::nats::service::nats_client& /*session*/) {
    auto menu = std::make_unique<cli::Menu>("script");
    menu->Insert("parse",
                 [](std::ostream& out, std::vector<std::string> args) { process_parse(out, args); },
                 "Parse a payoff script with ORE's grammar, offline",
                 {"<file>"});
    menu->Insert("check",
                 [](std::ostream& out, std::vector<std::string> args) { process_check(out, args); },
                 "Print a script's diagnostics as FILE:LINE:COLUMN: severity: message [code], "
                 "offline",
                 {"<file> [--variables <file>]"});
    menu->Insert(
        "inputs",
        [](std::ostream& out, std::vector<std::string> args) { process_inputs(out, args); },
        "List a script's inputs, or the merge of its variants' inputs, offline",
        {"<file> [<file>...] [--out <path>]"});
    menu->Insert("print",
                 [](std::ostream& out, std::vector<std::string> args) { process_print(out, args); },
                 "Print a script in the normal form ORE's converter writes, offline",
                 {"<file>"});
    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

}
