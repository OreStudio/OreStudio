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
#ifndef ORES_REPORTING_API_DOMAIN_ORE_FILE_VERSION_HPP
#define ORES_REPORTING_API_DOMAIN_ORE_FILE_VERSION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The contents of an ORE file as one engine version ships it.
 *
 * The contents of an ORE file as one version of the ORE engine ships it. The engine
 * ships some files itself, the XML schemas above all, and the schema of a version
 * is the one a document of that version must validate against: a schema of another
 * version may accept what the engine refuses, or refuse what it reads.
 *
 * One row holds one file at one engine version, with its digest and its size, so a
 * run's page can hand a person the schema of the engine that ran it. A schema that
 * includes others is complete only with them; the includes are read from the
 * contents, which state them, so the two cannot disagree.
 *
 * The seed loads the files vendored in external/ore for the engine version their
 * manifest states. A later engine version adds rows beside the earlier ones and
 * replaces none of them.
 */
struct ore_file_version final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this file at this version.
     */
    boost::uuids::uuid id;

    /**
     * @brief The ORE file whose contents these are, for example conventions_schema.
     */
    std::string ore_file_code;

    /**
     * @brief The version of the ORE engine that ships these contents, as the engine states it, for
     * example 1.8.17.0.
     *
     * A version and not a word, so a synthetic value takes no uniqueness suffix: the row is made
     * unique by the file it hangs off.
     */
    std::string engine_version;

    /**
     * @brief The contents of the file, as the engine ships it.
     */
    std::string content;

    /**
     * @brief The SHA-256 of the contents, in lower case hex.
     */
    std::string sha256;

    /**
     * @brief The size of the contents in bytes.
     */
    int bytes = 0;

    /**
     * @brief Username of the person who last modified this ORE file version.
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
    friend bool operator==(const ore_file_version&, const ore_file_version&) = default;
};

/**
 * @brief Dispatch-key identifier for ore_file_version, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ore_file_version&) {
    return "ores.reporting.ore_file_version";
}

}

#endif
