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
#ifndef ORES_ORE_CORE_RESULTS_OUTPUT_MANIFEST_HPP
#define ORES_ORE_CORE_RESULTS_OUTPUT_MANIFEST_HPP

#include "ores.ore.core/export.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::results {

/**
 * @brief One file the engine wrote, as the node uploaded it.
 */
struct output_manifest_file {
    /**
     * @brief The file's name, as the engine named it.
     */
    std::string name;
    /**
     * @brief The family the catalogue says the file belongs to, or empty.
     *
     * Empty when no family covers the file, so a reader sees what the run wrote
     * that this version does not model.
     */
    std::string family;
    /**
     * @brief The file's size as the engine wrote it.
     */
    std::int64_t bytes = 0;
    /**
     * @brief The size of the object the node uploaded.
     */
    std::int64_t compressed_bytes = 0;
    /**
     * @brief The SHA-256 of the file as the engine wrote it, in hex.
     */
    std::string sha256;
    /**
     * @brief The object key the file was uploaded under.
     */
    std::string key;
};

/**
 * @brief Every file one run of the engine wrote, and where each one is.
 *
 * The node writes it last, so a manifest that exists names files that exist. A
 * reader asks it which files it models and fetches those, so neither the size
 * of the run nor the files it does not model cost the reader anything.
 */
struct output_manifest {
    /**
     * @brief The compute result that wrote the files.
     */
    std::string result_id;
    /**
     * @brief The files, sorted by name.
     */
    std::vector<output_manifest_file> files;
};

/**
 * @brief The family code of a file the engine wrote, or empty when none covers it.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::string family_code_of(std::string_view file_name);

/**
 * @brief Whether the file belongs to a family whose data is market state.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT bool is_market_state_file(const output_manifest_file& file);

/**
 * @brief Whether the file belongs to a family whose data is a run result.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT bool is_result_file(const output_manifest_file& file);

/**
 * @brief The manifest as the JSON object the node uploads.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::string to_json(const output_manifest& manifest);

/**
 * @brief The manifest a JSON object states.
 *
 * @throws std::runtime_error when the text is not a manifest.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT output_manifest manifest_from_json(std::string_view text);

}

#endif
