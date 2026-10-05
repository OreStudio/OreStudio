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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_SERIES_ASSET_CLASS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_SERIES_ASSET_CLASS_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct market_series_asset_class_key {
    boost::uuids::uuid market_series_id;
    std::string asset_class_code;
};

struct market_series_asset_class_write {
    boost::uuids::uuid market_series_id;
    std::string asset_class_code;
};

struct market_series_asset_class_change {
    market_series_asset_class_write write;
    ores::utility::domain::precondition precondition;
};

struct market_series_asset_class_removal {
    market_series_asset_class_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct market_series_asset_class_lookup {
    market_series_asset_class_key key;
    std::optional<ores::marketdata::domain::market_series_asset_class> market_series_asset_class;
};

struct market_series_asset_classes_filter {
    std::optional<boost::uuids::uuid> market_series_id;
    std::optional<std::vector<boost::uuids::uuid>> market_series_id_one_of;
};

struct list_market_series_asset_classes_request {
    using response_type = struct list_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<market_series_asset_classes_filter> filter;
};

struct list_market_series_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_series_asset_class> market_series_asset_classes;
    std::uint64_t total;
};

struct get_market_series_asset_class_request {
    using response_type = struct get_market_series_asset_class_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_series_asset_class_key key;
};

struct get_market_series_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_series_asset_class> market_series_asset_class;
};

struct get_many_market_series_asset_classes_request {
    using response_type = struct get_many_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_series_asset_class_key> keys;
};

struct get_many_market_series_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<market_series_asset_class_lookup> entries;
};

struct put_market_series_asset_class_request {
    using response_type = struct put_market_series_asset_class_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_series_asset_class_change change;
    ores::utility::domain::change_intent intent;
};

struct put_market_series_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_series_asset_class> market_series_asset_class;
};

struct put_many_market_series_asset_classes_request {
    using response_type = struct put_many_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_series_asset_class_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_market_series_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_series_asset_class> market_series_asset_classes;
};

struct delete_market_series_asset_class_request {
    using response_type = struct delete_market_series_asset_class_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_series_asset_class_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_market_series_asset_class_response {
    ores::utility::domain::result result;
};

struct delete_many_market_series_asset_classes_request {
    using response_type = struct delete_many_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_series_asset_class_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_market_series_asset_classes_response {
    ores::utility::domain::result result;
};

struct list_by_market_series_id_market_series_asset_classes_request {
    using response_type = struct list_by_market_series_id_market_series_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_series_asset_classes.list_by_market_series_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid market_series_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<market_series_asset_classes_filter> filter;
};

struct list_by_market_series_id_market_series_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_series_asset_class> market_series_asset_classes;
    std::uint64_t total;
};

}

#endif
