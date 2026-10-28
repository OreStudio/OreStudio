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
#include "ores.reporting.core/results/canonical_log_reader.hpp"
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::results {
namespace {

constexpr std::string_view log_expected_header =
    "ordinal,severity,scope,message,subject,subject_id,cause,detail,event_time,source";

/**
 * @brief The records of a file, and what the scan refused.
 */
struct log_record_scan {
    /**
     * @brief The records, in the order the writer stated them.
     */
    std::vector<std::vector<std::string>> records;
    /**
     * @brief The place of the record a quote opens and never closes, from one,
     * or zero when every quote closes.
     */
    std::size_t unterminated = 0;
};

/**
 * @brief Splits the file into records by the writer's own rules.
 *
 * A record ends at a newline that is not inside a quoted field, because the
 * writer states a message whole: a message that carries a newline is written
 * between quotes and the newline is part of it. Splitting the file at the first
 * newline would cut such a message into rows that no longer state nine fields.
 *
 * The writer doubles a quote inside a quoted field, as CSV does, so a pair reads
 * as the one character it stands for rather than as two field boundaries.
 */
log_record_scan log_split_records(std::string_view csv) {
    log_record_scan scan;
    std::vector<std::string> fields;
    std::string field;
    bool quoted = false;

    for (std::size_t i = 0; i < csv.size(); ++i) {
        const auto c = csv[i];
        if (c == '"') {
            if (quoted && i + 1 < csv.size() && csv[i + 1] == '"') {
                field.push_back('"');
                ++i;
            } else {
                quoted = !quoted;
            }
        } else if (c == ',' && !quoted) {
            fields.push_back(std::move(field));
            field.clear();
        } else if (c == '\n' && !quoted) {
            fields.push_back(std::move(field));
            field.clear();
            if (!fields.back().empty() && fields.back().back() == '\r')
                fields.back().pop_back();
            scan.records.push_back(std::move(fields));
            fields.clear();
        } else {
            field.push_back(c);
        }
    }

    if (quoted) {
        scan.unterminated = scan.records.size() + 1;
        return scan;
    }
    if (!field.empty() || !fields.empty()) {
        fields.push_back(std::move(field));
        if (!fields.back().empty() && fields.back().back() == '\r')
            fields.back().pop_back();
        scan.records.push_back(std::move(fields));
    }
    return scan;
}

/** The record as the line it was written from, to compare with the header. */
std::string log_join_record(const std::vector<std::string>& fields) {
    std::string out;
    for (std::size_t i = 0; i < fields.size(); ++i) {
        if (i != 0)
            out.push_back(',');
        out += fields[i];
    }
    return out;
}

}

log_read_outcome read_canonical_log_csv(std::string_view csv) {
    log_read_outcome outcome;

    if (csv.substr(0, 3) == "\xEF\xBB\xBF")
        csv.remove_prefix(3);
    if (csv.empty()) {
        outcome.refusals.push_back("the log rows are empty.");
        return outcome;
    }

    const auto scan = log_split_records(csv);
    if (scan.unterminated != 0) {
        outcome.refusals.push_back("record " + std::to_string(scan.unterminated) +
                                   " opens a quoted field and never closes it.");
        return outcome;
    }
    if (scan.records.empty()) {
        outcome.refusals.push_back("the log rows are empty.");
        return outcome;
    }

    const auto stated_header = log_join_record(scan.records.front());
    if (stated_header != log_expected_header) {
        outcome.refusals.push_back("the log rows state a header of '" + stated_header + "', not '" +
                                   std::string(log_expected_header) + "'.");
        return outcome;
    }

    for (std::size_t i = 1; i < scan.records.size(); ++i) {
        const auto& fields = scan.records[i];
        const auto record_number = i + 1;
        if (fields.size() == 1 && fields[0].find_first_not_of(" \t") == std::string::npos)
            continue;
        if (fields.size() != 10) {
            outcome.refusals.push_back("record " + std::to_string(record_number) + " states " +
                                       std::to_string(fields.size()) + " fields, not ten.");
            continue;
        }
        if (fields[1].empty() || fields[3].empty()) {
            outcome.refusals.push_back("record " + std::to_string(record_number) +
                                       " states no severity or no message.");
            continue;
        }

        service::engine_log_row row;
        row.severity = fields[1];
        row.scope = fields[2];
        row.message = fields[3];
        row.subject = fields[4];
        row.subject_id = fields[5];
        row.cause = fields[6];
        row.detail = fields[7];
        row.event_time = fields[8];
        row.source = fields[9];
        outcome.rows.push_back(std::move(row));
    }

    return outcome;
}

}
