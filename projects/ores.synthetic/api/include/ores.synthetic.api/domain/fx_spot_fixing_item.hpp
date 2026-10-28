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
#ifndef ORES_SYNTHETIC_API_DOMAIN_FX_SPOT_FIXING_ITEM_HPP
#define ORES_SYNTHETIC_API_DOMAIN_FX_SPOT_FIXING_ITEM_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief One FX fixing an FX spot feed may start from.
 *
 * One FX fixing that an fx_spot_generation_config whose price_source is
 * fixing may start from. A feed reads its items in rank order and starts from the
 * first that has a fixing: the latest on or before the date of the market clock, or
 * on a stated date. The window sets how many days of fixings measure the
 * volatility. An item may quote the pair the other way round, and the feed then reads
 * it as one over the value. Party- and tenant-scoped.
 */
struct fx_spot_fixing_item final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this fixing item.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this item's configuration belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent fx_spot_generation_config this item may start.
     */
    boost::uuids::uuid fx_spot_config_id;

    /**
     * @brief Order in which the feed tries its items, lowest first.
     */
    int rank = 0;

    /**
     * @brief The fixing's oresmd URI, the ORE index, such as
     * =oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD=.
     */
    std::string item;

    /**
     * @brief The provider to read the item from, or empty when one provider carries it.
     */
    std::string provider_code = "";

    /**
     * @brief Which fixing to start from: latest_on_or_before_clock, the latest on or before the
     * date of the market clock, so a feed that starts again takes the newest value; or
     * exact:<YYYY-MM-DD>, the fixing of that date.
     */
    std::string policy = "latest_on_or_before_clock";

    /**
     * @brief How many calendar days of fixings, ending on the starting fixing, measure the
     * volatility.
     */
    int window_days = 90;

    /**
     * @brief Username of the person who last modified this FX spot fixing item.
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
    friend bool operator==(const fx_spot_fixing_item&, const fx_spot_fixing_item&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_spot_fixing_item, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_spot_fixing_item&) {
    return "ores.synthetic.fx_spot_fixing_item";
}

}

#endif
