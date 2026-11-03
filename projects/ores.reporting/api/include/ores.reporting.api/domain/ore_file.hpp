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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_API_DOMAIN_ORE_FILE_HPP
#define ORES_REPORTING_API_DOMAIN_ORE_FILE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A file of an ORE run, as the ORE user guide identifies it.
 *
 * Reference data naming the files an ORE run reads and writes, one row for each
 * file as the ORE user guide identifies it: the market data, the fixing history, the
 * NPV report, the NPV cube. The code is the guide's name for the file, so it says
 * what the file is and not what a run happened to call it.
 *
 * A file is not a parameter. Several parameters may name the same file: the NPV
 * cube is written by the cubeFile of simulation and read by the cubeFile of xva and
 * pfe. Each of those is a file type, and each file type names its file by this
 * code. A file no parameter names, such as the master input ore.xml or the log, is
 * still a file.
 *
 * The table recognises a file by its name. The typical name is the name ORE gives
 * the file when nothing says otherwise, and the name ORE Studio writes when a run
 * has one file of the kind. The regular expression matches every name a run may
 * give it, so a file a run wrote can be told apart without knowing which parameter
 * named it.
 *
 * The table describes files. It does not hold the content of a file.
 */
struct ore_file final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The stable code of the file, from the name the ORE user guide gives it, in lower case
     * with underscores: market_data, fixing_history, npv_report, npv_cube.
     */
    std::string code;

    /**
     * @brief The name of the file as the ORE user guide gives it, for example NPV Cube or Fixing
     * History.
     */
    std::string name;

    /**
     * @brief The category the file belongs to, such as configuration, market data or the valuation
     * reports.
     */
    std::string category_code;

    /**
     * @brief For a configuration document, the configuration type whose entities fill it. Nothing
     * for any other file.
     */
    std::optional<std::string> configuration_type_code;

    /**
     * @brief The label of the section of the ORE user guide that describes the file, for example
     * sec:market_data. Nothing when the guide has no section for it.
     */
    std::optional<std::string> guide_section;

    /**
     * @brief Where the file comes from in a run: supplied when the run is given it, produced when
     * ORE writes it, packaged when ORE Studio makes it to carry a run to the engine and back. A
     * file one analytic produces and another reads is produced; the file types of the two
     * parameters say which reads and which writes.
     */
    std::string origin;

    /**
     * @brief The format of the file: xml, csv, csv_gz for a compressed csv, txt, dat for a binary
     * file, json, json_gz for a compressed json, tar_gz for a compressed archive of other files, or
     * xsd for an XML schema. An xml file is validated against the schema schema_ore_file_code
     * names.
     */
    std::string format;

    /**
     * @brief For an xml file, the ORE file of the XSD that declares its root element, for example
     * conventions_schema for the conventions. The schema is itself an ORE file, so its contents can
     * be read for each engine version. Nothing for a file that is not xml, or an xml file that ORE
     * publishes no schema for.
     */
    std::optional<std::string> schema_ore_file_code;

    /**
     * @brief The name ORE gives the file when nothing names it otherwise, and the name ORE Studio
     * writes when a run has one file of the kind: npv.csv, cube.csv.gz. For a file a run writes
     * once for each trade or netting set, an example of such a name.
     */
    std::string typical_filename;

    /**
     * @brief The regular expression every name a run gives the file matches, anchored at both ends:
     * ^npv\.csv$, ^exposure_trade_.+\.csv$. No name a run writes matches two files.
     */
    std::string filename_regex;

    /**
     * @brief What the file holds and what a run does with it, for a reader who has never seen an
     * ORE run.
     */
    std::string description;

    /**
     * @brief The order the file appears in a list.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this ORE file.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const ore_file&, const ore_file&) = default;
};

/**
 * @brief Dispatch-key identifier for ore_file, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ore_file&) {
    return "ores.reporting.ore_file";
}

}

#endif
