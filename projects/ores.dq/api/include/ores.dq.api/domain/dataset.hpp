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
#ifndef ORES_DQ_API_DOMAIN_DATASET_HPP
#define ORES_DQ_API_DOMAIN_DATASET_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Central dataset registry with full lineage.
 *
 * Central dataset registry with full lineage. Tracks provenance through
 * upstream_derivation_id, and lineage_depth is calculated from the hierarchy.
 *
 * A dataset is identified two ways: by its code, and by its name within a
 * subject area and domain. The table enforces both, so the model states code as
 * the natural key and declares the three-column key as an extra unique index.
 *
 * The columns that point at other entities -- coding_scheme_code, origin_code,
 * nature_code, treatment_code, methodology_id and upstream_derivation_id --
 * are plain columns rather than declared references, because the table carries no
 * constraint and adding one would refuse rows the platform accepts today.
 */
struct dataset final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this dataset.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique code for stable referencing.
     *
     * Examples: "iso.currencies", "fpml.codelists".
     */
    std::string code;

    /**
     * @brief Optional catalog this dataset belongs to.
     */
    std::optional<std::string> catalog_name;

    /**
     * @brief Subject area the dataset belongs to; half of the three-column key.
     */
    std::string subject_area_name;

    /**
     * @brief Domain the subject area sits in; the other half.
     */
    std::string domain_name;

    /**
     * @brief Optional coding scheme the dataset's codes come from.
     */
    std::optional<std::string> coding_scheme_code;

    /**
     * @brief Origin dimension this dataset's values came from.
     */
    std::string origin_code;

    /**
     * @brief Nature dimension of this dataset's values.
     */
    std::string nature_code;

    /**
     * @brief Treatment dimension of this dataset's values.
     */
    std::string treatment_code;

    /**
     * @brief Optional methodology that produces the dataset.
     */
    std::optional<boost::uuids::uuid> methodology_id;

    /**
     * @brief Human-readable name for the dataset.
     */
    std::string name;

    /**
     * @brief Detailed description of the dataset's contents.
     */
    std::string description;

    /**
     * @brief The system the dataset was sourced from.
     */
    std::string source_system_id;

    /**
     * @brief Why the business holds this dataset.
     */
    std::string business_context;

    /**
     * @brief Optional dataset this one derives from.
     */
    std::optional<boost::uuids::uuid> upstream_derivation_id;

    /**
     * @brief Depth of this dataset in the derivation hierarchy.
     */
    int lineage_depth = 0;

    /**
     * @brief The date the dataset's contents are stated as of.
     */
    std::chrono::system_clock::time_point as_of_date;

    /**
     * @brief When the dataset was ingested.
     */
    std::chrono::system_clock::time_point ingestion_timestamp;

    /**
     * @brief Optional licensing terms for the dataset.
     */
    std::optional<std::string> license_info;

    /**
     * @brief The artefact type this dataset is published as.
     */
    std::string artefact_type;

    /**
     * @brief Username of the person who last modified this dataset.
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
    friend bool operator==(const dataset&, const dataset&) = default;
};

/**
 * @brief Dispatch-key identifier for dataset, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const dataset&) {
    return "ores.dq.dataset";
}

}

#endif
