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
#ifndef ORES_SYNTHETIC_API_DOMAIN_FOLDER_HPP
#define ORES_SYNTHETIC_API_DOMAIN_FOLDER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A generic hierarchy node organizing feeds into collection/asset-class/instrument-type
 * folders.
 *
 * A generic, self-referencing hierarchy node. The Market Simulator tree is
 * drawn directly from these rows rather than parsed out of a feed's
 * source_name string: one root folder per party ("Synthetic"), with
 * market_data_generation_config collections, asset-class folders (e.g.
 * "FX"), and instrument-type folders (e.g. "FX Rates") nested beneath it.
 * fx_spot_generation_config rows reference their immediate parent folder
 * via folder_id. kind distinguishes what a given row represents; =
 * collection_id= is populated only for kind  'collection'= rows, linking
 * back to the market_data_generation_config it represents (that entity
 * remains the source of truth for name/description/enabled/dataset_id;
 * this row's own name is kept in sync for uniform hierarchy display).
 *
 * Having real, queryable folder rows (rather than string-parsing) lets any
 * caller -- Qt, ores.shell, or a wt workflow -- resolve "everything under
 * this folder" the same way, via the generated hierarchy function, instead
 * of each reimplementing the tree-walk itself.
 */
struct folder final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this folder node.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this folder belongs to. Not a natural key: a party has
     * many folders (one root, several collections, and their asset-class/instrument-type children),
     * so no uniqueness is enforced beyond the surrogate id.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent folder, for the tree hierarchy. Null for the root folder.
     */
    std::optional<boost::uuids::uuid> parent_id;

    /**
     * @brief Display name of this folder (e.g. "Synthetic", "Basic", "FX", "FX Rates").
     */
    std::string name;

    /**
     * @brief What this folder represents: "root", "collection", "asset_class", or
     * "instrument_type".
     */
    std::string kind;

    /**
     * @brief For kind  'collection'= rows only: the market_data_generation_config this folder
     * represents. Null for every other kind.
     */
    std::optional<boost::uuids::uuid> collection_id;

    /**
     * @brief Username of the person who last modified this folder.
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
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for folder, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const folder&) {
    return "ores.synthetic.folder";
}

}

#endif
