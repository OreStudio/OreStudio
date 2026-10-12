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
#ifndef ORES_REPORTING_API_DOMAIN_FILE_TYPE_HPP
#define ORES_REPORTING_API_DOMAIN_FILE_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A kind of file in the ORE universe and its well known name.
 *
 * Reference data naming the files an ORE run reads and writes. A file type is one
 * parameter of the run document that carries a file name: a Setup parameter such as
 * conventionsFile, or the parameter of an analytic such as the outputFileName of npv.
 * The table gives each its well known name, so a configuration never repeats a file
 * name and the export looks the name up when it writes a file.
 *
 * The names come from the shipped ORE samples. Each is the most common name the samples
 * give the file, and sample_count out of sample_total records how many use it.
 * Names that embed a date are set aside, because a date is a value of the run.
 *
 * A name may be a pattern with a key, for example portfolio_{{BOOK_ID}}.xml, for a file
 * that a run splits. The table can hold the pattern. Splitting is a later task.
 *
 * The table lists names and patterns. It does not hold the content of a file.
 */
struct file_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The stable code of the file type: the scope and the ORE parameter, for example
     * setup.conventionsFile or npv.outputFileName.
     */
    std::string code;

    /**
     * @brief The analytic whose parameter names the file, or nothing when the Setup block names it.
     */
    std::optional<std::string> analytic_type_code;

    /**
     * @brief The ORE parameter that carries the file name.
     */
    std::string ore_parameter;

    /**
     * @brief What the file is: input_document, data_file, output_file or intermediate_file. An
     * intermediate file is one analytic writes and another reads.
     */
    std::string kind;

    /**
     * @brief The configuration type whose entities fill the document, or nothing when no
     * configuration type describes it yet.
     */
    std::optional<std::string> configuration_type_code;

    /**
     * @brief The well known name of the file, or a pattern when a run may split the file by a key.
     * A pattern writes the key between double braces, for example portfolio_{{BOOK_ID}}.xml. The
     * key is upper case letters, digits and underscores.
     */
    std::string name_pattern;

    /**
     * @brief How many of the ORE samples that name this file give it this name.
     */
    int sample_count = 0;

    /**
     * @brief How many of the ORE samples name this file at all.
     */
    int sample_total = 0;

    /**
     * @brief What the file is, in one line.
     */
    std::string description;

    /**
     * @brief The order the file type appears in a list.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this file type.
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
    friend bool operator==(const file_type&, const file_type&) = default;
};

/**
 * @brief Dispatch-key identifier for file_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const file_type&) {
    return "ores.reporting.file_type";
}

}

#endif
