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
#include "ores.marketdata.core/service/market_state_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.ore.core/results/payload_index.hpp"
#include "ores.utility/compression/gzip.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <boost/json.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <cstdint>
#include <format>
#include <span>

namespace ores::marketdata::service {
namespace {

// The headings as one JSON array, in the order the file states them.
std::string columns_json(const std::vector<std::string>& columns) {
    boost::json::array array;
    for (const auto& column : columns)
        array.emplace_back(column);
    return boost::json::serialize(array);
}

// The keys as one JSON object of name to array, so a reader finds the dates, the
// scenarios or the factors it wants without unpacking the payload.
std::string keys_json(const std::vector<std::pair<std::string, std::vector<std::string>>>& keys) {
    boost::json::object object;
    for (const auto& [name, values] : keys) {
        boost::json::array array;
        for (const auto& value : values)
            array.emplace_back(value);
        object[name] = array;
    }
    return boost::json::serialize(object);
}

}

market_state_outcome read_market_state(const std::vector<market_state_file>& files,
                                       const market_state_run& run) {
    market_state_outcome outcome;

    for (const auto& file : files) {
        const auto* family = ores::ore::results::family_of_file(file.name);
        if (family == nullptr || !ores::ore::results::is_market_state(*family)) {
            outcome.left_alone.push_back(file.name);
            continue;
        }

        std::vector<std::string> refusals;
        const auto index = ores::ore::results::index_payload(file.body, *family, refusals);
        for (const auto& refusal : refusals)
            outcome.refusals.push_back(file.name + ": " + refusal);
        if (!refusals.empty())
            continue;

        // The payload is the file as the engine wrote it, compressed before it is
        // encoded, so the cost of holding it as text is a third of the compressed
        // size and not of the file.
        const auto compressed = ores::utility::compression::gzip_compress(
            std::span<const char>(file.body.data(), file.body.size()));
        const auto encoded = ores::utility::convert::base64_converter::convert(
            std::vector<std::uint8_t>(compressed.begin(), compressed.end()));

        outcome.payloads.push_back(domain::market_state_payload{
            .id = boost::uuids::random_generator()(),
            .party_id = run.party_id,
            .report_instance_id = run.report_instance_id,
            .family = family->code,
            .file_name = file.name,
            .as_of = run.as_of,
            .report_definition_id = run.report_definition_id,
            .row_count = static_cast<int>(index.rows),
            .column_count = static_cast<int>(index.columns.size()),
            .column_names = columns_json(index.columns),
            .key_values = keys_json(index.keys),
            .keys_truncated = index.truncated,
            .payload = encoded,
            .payload_bytes = static_cast<int>(compressed.size()),
            .content_sha256 = ores::utility::crypto::sha256::hex_digest(file.body),
            .source = family->code});
    }

    return outcome;
}

}
