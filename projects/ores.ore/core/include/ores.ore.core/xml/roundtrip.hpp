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
#ifndef ORES_ORE_CORE_XML_ROUNDTRIP_HPP
#define ORES_ORE_CORE_XML_ROUNDTRIP_HPP

#include "ores.ore.core/export.hpp"
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief The ORE document kinds the XML facet reads and writes.
 */
enum class document_kind {
    portfolio,
    currency_config,
    calendar_adjustments,
    conventions,
};

/**
 * @brief Names the document kind a file holds, from its root element.
 *
 * The root element alone decides. Searching the header for a keyword
 * instead would read a curve configuration that names a conventions block
 * in its opening lines as a conventions document, and rewrite it as an
 * empty one.
 *
 * @param path Path to the file
 * @return The kind, or @c std::nullopt when the root element is not one of
 * them
 */
ORES_ORE_CORE_EXPORT std::optional<document_kind>
detect_document_kind(const std::filesystem::path& path);

/**
 * @brief What one walk over a directory did.
 *
 * Unsupported and failed are distinct counts on purpose. A walk that folds
 * both into one number cannot tell a caller that it dropped a document
 * whose kind it understands.
 */
struct roundtrip_summary {
    int total_xml_files = 0;      ///< every .xml seen under the input directory
    int unsupported = 0;          ///< root element is not a round-trippable document
    int failed = 0;               ///< the import or the export threw
    int output_files_written = 0; ///< documents written under the output directory
    int trades_mapped = 0;
    int trades_passthrough = 0;
    int currency_files = 0;
    int calendar_files = 0;
    int convention_files = 0;
    std::vector<std::string> failures; ///< "<relative path>: <what failed>", in walk order
    long long import_ms = 0;           ///< wall time inside importer calls
    long long export_ms = 0;           ///< wall time inside exporter calls
    long long total_ms = 0;            ///< wall time for the full walk
};

/**
 * @brief Round trips every supported document under @p input_dir, mirroring
 * the outputs under @p output_dir.
 *
 * Each document is imported into the domain and exported back. The walk
 * composes the importer and the exporter and owns neither: it is the only
 * unit that knows both.
 *
 * Unsupported documents are counted and not written. A document whose
 * import or export throws is counted as failed, with its message kept in
 * @c failures, so a caller can see what was lost rather than infer it from
 * a counter.
 *
 * A document can export without error and still lose fidelity. That is not
 * visible from here. Re-import the output to check it.
 *
 * No database or network access; purely file-level.
 *
 * @param input_dir Directory to walk recursively
 * @param output_dir Directory to mirror the outputs under
 * @return The walk's counts, timings and failures
 */
ORES_ORE_CORE_EXPORT roundtrip_summary
roundtrip(const std::filesystem::path& input_dir, const std::filesystem::path& output_dir);

}

#endif
