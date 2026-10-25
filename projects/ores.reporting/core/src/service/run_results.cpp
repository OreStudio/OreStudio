/** -*- mode: c++; tab-width: 4; indent-tabs-mode: nil -*-
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
 *
 */
#include "ores.reporting.core/service/run_results.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.platform/filesystem/scoped_temp_file.hpp"
#include "ores.storage.core/filesystem/archiver.hpp"
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <optional>
#include <string_view>

namespace ores::reporting::service {

namespace {

namespace fs = std::filesystem;

constexpr std::string_view results_file_name = "npv.csv";

/// The column names the result carries, in the order the screen reads them.
constexpr std::string_view trade_id_column = "TradeId";
constexpr std::string_view trade_type_column = "TradeType";
constexpr std::string_view npv_column = "NPV";
constexpr std::string_view npv_base_column = "NPV(Base)";
constexpr std::string_view notional_column = "Notional";
constexpr std::string_view counterparty_column = "CounterParty";

std::string lower_of(std::string_view text) {
    std::string lowered(text);
    std::ranges::transform(lowered, lowered.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return lowered;
}

/// Splits one CSV line, honouring the quotes the engine writes around a name
/// that holds a comma.
std::vector<std::string> split_csv(std::string_view line) {
    std::vector<std::string> fields;
    std::string field;
    bool quoted = false;
    for (std::size_t i = 0; i < line.size(); ++i) {
        const char c = line[i];
        if (quoted) {
            if (c == '"' && i + 1 < line.size() && line[i + 1] == '"') {
                field.push_back('"');
                ++i;
            } else if (c == '"') {
                quoted = false;
            } else {
                field.push_back(c);
            }
            continue;
        }
        if (c == '"') {
            quoted = true;
        } else if (c == ',') {
            fields.push_back(std::move(field));
            field.clear();
        } else if (c != '\r') {
            field.push_back(c);
        }
    }
    fields.push_back(std::move(field));
    return fields;
}

std::optional<std::size_t> column_of(const std::vector<std::string>& header,
                                     std::string_view name) {
    const auto found = std::ranges::find(header, name);
    if (found == header.end())
        return std::nullopt;
    return static_cast<std::size_t>(std::distance(header.begin(), found));
}

std::string field_at(const std::vector<std::string>& fields,
                     const std::optional<std::size_t>& column) {
    if (!column || *column >= fields.size())
        return {};
    return fields[*column];
}

fs::path results_path_in(const fs::path& output) {
    std::error_code ec;
    for (fs::recursive_directory_iterator it(output, ec), end; !ec && it != end; it.increment(ec)) {
        if (!it->is_regular_file())
            continue;
        if (lower_of(it->path().filename().string()) == results_file_name)
            return it->path();
    }
    return {};
}

}

run_results_read read_run_results_from_archive(std::span<const std::byte> archive_bytes) {
    run_results_read read;

    const ores::platform::filesystem::scoped_temp_file archive;
    const ores::platform::filesystem::scoped_temp_directory output;
    try {
        {
            std::ofstream stream(archive.path(), std::ios::binary);
            if (!stream)
                throw std::runtime_error("Cannot write the run's output archive to " +
                                         archive.path().string());
            stream.write(reinterpret_cast<const char*>(archive_bytes.data()),
                         static_cast<std::streamsize>(archive_bytes.size()));
        }

        ores::storage::filesystem::archiver::extract(archive.path(), output.path());
    } catch (const std::exception& e) {
        read.error = std::format("The run's output was not read: {}", e.what());
        return read;
    }

    const auto results = results_path_in(output.path());
    if (results.empty()) {
        read.error = std::format("The run's output holds no {}, so its results were not read.",
                                 results_file_name);
        return read;
    }

    std::ifstream stream(results, std::ios::binary);
    if (!stream) {
        read.error = std::format("The run's {} could not be opened.", results_file_name);
        return read;
    }

    std::string line;
    if (!std::getline(stream, line)) {
        read.error = std::format("The run's {} is empty.", results_file_name);
        return read;
    }
    const auto header = split_csv(line);
    const auto trade_id = column_of(header, trade_id_column);
    const auto npv = column_of(header, npv_column);
    if (!trade_id || !npv) {
        read.error = std::format("The run's {} does not carry {} and {}, so its results were not "
                                 "read.",
                                 results_file_name,
                                 trade_id_column,
                                 npv_column);
        return read;
    }
    const auto trade_type = column_of(header, trade_type_column);
    const auto npv_base = column_of(header, npv_base_column);
    const auto notional = column_of(header, notional_column);
    const auto counterparty = column_of(header, counterparty_column);

    while (std::getline(stream, line)) {
        if (line.find_first_not_of(" \t\r\n") == std::string::npos)
            continue;
        const auto fields = split_csv(line);
        run_result_row row;
        row.trade_id = field_at(fields, trade_id);
        row.trade_type = field_at(fields, trade_type);
        row.npv = field_at(fields, npv);
        row.npv_base = field_at(fields, npv_base);
        row.notional = field_at(fields, notional);
        row.counterparty = field_at(fields, counterparty);
        if (!row.trade_id.empty())
            read.rows.push_back(std::move(row));
    }

    return read;
}

}
