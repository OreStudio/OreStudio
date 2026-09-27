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
#ifndef ORES_DQ_API_DOMAIN_PUBLICATION_HPP
#define ORES_DQ_API_DOMAIN_PUBLICATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <cstdint>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief One row per dataset publication, recording where it went and what it wrote.
 *
 * An append-only log of datasets published to production tables: which dataset
 * went where, under which mode, and how many records each outcome touched. The
 * publish path writes one row per dataset it dispatches.
 *
 * The table is called ores_dq_dataset_publications_tbl, so the model states its
 * table name rather than deriving it: the entity is a publication, and the table
 * names the thing published.
 *
 * It is a run log, not an entity with a lifecycle. Nothing edits a row after it is
 * written, so the model declares :current_state:: no transaction-time pair, no
 * exclusion constraint, no version and no audit tail. Rows carry their own
 * published_at, which is the ordering the reads use. The surrogate id is the
 * primary key because a dataset may be published many times.
 */
struct publication final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate identifier for this publication record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The dataset that was published.
     */
    boost::uuids::uuid dataset_id;

    /**
     * @brief The dataset's code at the time of publication, recorded so the row still reads after
     * the dataset is renamed or removed.
     */
    std::string dataset_code;

    /**
     * @brief How records were written to the target table: upsert, insert_only or replace_all.
     */
    std::string mode;

    /**
     * @brief The production table the dataset was published into.
     */
    std::string target_table;

    /**
     * @brief Records the publication inserted.
     */
    std::int64_t records_inserted;

    /**
     * @brief Records the publication updated.
     */
    std::int64_t records_updated;

    /**
     * @brief Records the publication left alone.
     */
    std::int64_t records_skipped;

    /**
     * @brief Records the publication removed.
     */
    std::int64_t records_deleted;

    /**
     * @brief Who asked for the publication.
     */
    std::string published_by;

    /**
     * @brief When the publication was recorded. This is the column the log is read by.
     */
    std::chrono::system_clock::time_point published_at;

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
    friend bool operator==(const publication&, const publication&) = default;
};

/**
 * @brief Dispatch-key identifier for publication, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const publication&) {
    return "ores.dq.publication";
}

}

#endif
