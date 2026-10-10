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
#include "ores.shell/app/commands/marketdata/oresmd_index_commands.hpp"
#include "ores.marketdata.core/datum/ore_index_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include <algorithm>
#include <cli/cli.h>
#include <expected>
#include <fstream>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

namespace ores::shell::app::commands {

namespace {

/// One resolved row: the ORE index name and the oresmd URI it writes.
using index_row = std::pair<std::string, std::string>;

/**
 * @brief The oresmd URI of @p name, or why the codec refused it.
 *
 * The chain is the one the convention seed documents: read the ORE name into
 * a market index, then write the index as a fixing URI. Either codec may
 * refuse; both refusals are named so a caller never sees an empty URI.
 */
std::expected<std::string, std::string> uri_for(const std::string& name) {
    const auto index = ores::marketdata::datum::ore_index_codec::read(name);
    if (!index)
        return std::unexpected("ORE index codec: " + index.error());

    const auto uri = ores::marketdata::datum::oresmd_uri_codec::write_index(*index);
    if (!uri)
        return std::unexpected("oresmd URI codec: " + uri.error());

    return *uri;
}

/**
 * @brief Read every name, resolve it, and return the sorted rows or the
 * failures.
 *
 * Nothing is written until every name has resolved, so a rejected name
 * leaves no partial table behind for a consumer to trust.
 */
std::vector<index_row> read_index_rows(std::istream& in,
                                       const std::string& source,
                                       std::ostream& out,
                                       std::size_t& rejected) {
    std::vector<index_row> rows;
    std::string line;
    std::size_t line_number = 0;
    rejected = 0;
    while (std::getline(in, line)) {
        ++line_number;
        if (!line.empty() && line.back() == '\r')
            line.pop_back();
        if (line.empty() || line[0] == '#')
            continue;

        const auto uri = uri_for(line);
        if (!uri) {
            fail(out) << source << ":" << line_number << ": index '" << line
                      << "' has no oresmd URI (" << uri.error() << ")" << std::endl;
            ++rejected;
            continue;
        }
        rows.emplace_back(line, *uri);
    }

    std::sort(rows.begin(), rows.end());
    rows.erase(std::unique(rows.begin(), rows.end()), rows.end());
    return rows;
}

}

void oresmd_index_commands::process(std::ostream& out, const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "in", .requires_value = true, .default_value = ""},
                              {.name = "out", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    const auto& in_path = parsed->flag("in");
    const auto& out_path = parsed->flag("out");

    std::ifstream file;
    if (!in_path.empty() && in_path != "-") {
        file.open(in_path);
        if (!file) {
            fail(out) << "Cannot open " << in_path << std::endl;
            return;
        }
    }
    std::istream& in = file.is_open() ? static_cast<std::istream&>(file) : std::cin;
    const std::string source = in_path.empty() ? std::string("<stdin>") : in_path;

    std::size_t rejected = 0;
    const auto rows = read_index_rows(in, source, out, rejected);
    if (rejected != 0) {
        fail(out) << rejected << " index name(s) rejected; no output written." << std::endl;
        return;
    }

    if (!out_path.empty() && out_path != "-") {
        std::ofstream output(out_path);
        if (!output) {
            fail(out) << "Cannot write " << out_path << std::endl;
            return;
        }
        for (const auto& [name, uri] : rows)
            output << name << '\t' << uri << '\n';
        output.flush();
        if (!output) {
            fail(out) << "Failed to write " << out_path << std::endl;
            return;
        }
        out << "✓ Wrote " << rows.size() << " oresmd URI(s) to " << out_path << std::endl;
        out << "✓ Resolved " << rows.size() << " ORE index name(s)." << std::endl;
    } else {
        for (const auto& [name, uri] : rows)
            out << name << '\t' << uri << '\n';
    }
}

void oresmd_index_commands::register_verb(cli::Menu& marketdata_menu) {
    marketdata_menu.Insert("oresmd-index",
                           [](std::ostream& out, std::vector<std::string> args) {
                               process(std::ref(out), std::move(args));
                           },
                           "Turn ORE index names (one per line; blank and '#' lines skipped; stdin "
                           "when --in is unset) into oresmd fixing URIs, offline",
                           {"[--in <path>] [--out <path>]"});
}

}
