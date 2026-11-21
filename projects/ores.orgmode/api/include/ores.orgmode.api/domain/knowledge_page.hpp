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
#ifndef ORES_ORGMODE_API_DOMAIN_KNOWLEDGE_PAGE_HPP
#define ORES_ORGMODE_API_DOMAIN_KNOWLEDGE_PAGE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::orgmode::domain {

/**
 * @brief An org page of the knowledge base, as loaded
 *
 * A page of ORE's knowledge base, exactly as it was loaded: a user-facing org
 * file, its org id, where it lives in the repository, and its text, unchanged. A
 * page is user-facing when its frontmatter says #+audience: user. The pages are
 * installation data: they are published from DQ into the system tenant at
 * bring-up, and every tenant reads them there. Nothing here is parsed; the
 * indexing step reads the text and writes the sections the search reads.
 */
struct knowledge_page final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The page's org id, its :ID: property: the key every link between pages uses.
     */
    std::string id;

    /**
     * @brief Where the page lives, relative to the repository root, for example
     * doc/knowledge/domain/volatility.org.
     */
    std::string path;

    /**
     * @brief The page's org text, as the file holds it.
     */
    std::string org;

    /**
     * @brief Username of the person who last modified this knowledge page.
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
    friend bool operator==(const knowledge_page&, const knowledge_page&) = default;
};

/**
 * @brief Dispatch-key identifier for knowledge_page, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const knowledge_page&) {
    return "ores.orgmode.knowledge_page";
}

}

#endif
