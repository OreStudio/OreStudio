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
#ifndef ORES_TRADING_API_MESSAGING_PRODUCT_TYPE_ASSET_CLASS_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_PRODUCT_TYPE_ASSET_CLASS_PROTOCOL_HPP

#include "ores.trading.api/domain/product_type_asset_class.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct product_type_asset_class_key {
    std::string product_type_code;
    std::string asset_class_code;
};

struct product_type_asset_class_write {
    std::string product_type_code;
    std::string asset_class_code;
    bool is_home;
};

struct product_type_asset_class_change {
    product_type_asset_class_write write;
    ores::utility::domain::precondition precondition;
};

struct product_type_asset_class_removal {
    product_type_asset_class_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct product_type_asset_class_lookup {
    product_type_asset_class_key key;
    std::optional<ores::trading::domain::product_type_asset_class> product_type_asset_class;
};

struct product_type_asset_classes_filter {
    std::optional<std::string> product_type_code;
    std::optional<std::vector<std::string>> product_type_code_one_of;
};

struct product_type_asset_class_event {
    boost::uuids::uuid event_id;
    product_type_asset_class_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_product_type_asset_classes_request {
    using response_type = struct list_product_type_asset_classes_response;
    static constexpr std::string_view nats_subject = "trading.v1.product_type_asset_classes.list";
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
    std::optional<product_type_asset_classes_filter> filter;
};

struct list_product_type_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::product_type_asset_class> product_type_asset_classes;
    std::uint64_t total;
};

struct get_product_type_asset_class_request {
    using response_type = struct get_product_type_asset_class_response;
    static constexpr std::string_view nats_subject = "trading.v1.product_type_asset_classes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    product_type_asset_class_key key;
};

struct get_product_type_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::product_type_asset_class> product_type_asset_class;
};

struct get_many_product_type_asset_classes_request {
    using response_type = struct get_many_product_type_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.product_type_asset_classes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<product_type_asset_class_key> keys;
};

struct get_many_product_type_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<product_type_asset_class_lookup> entries;
};

struct put_product_type_asset_class_request {
    using response_type = struct put_product_type_asset_class_response;
    static constexpr std::string_view nats_subject = "trading.v1.product_type_asset_classes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    product_type_asset_class_change change;
    ores::utility::domain::change_intent intent;
};

struct put_product_type_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::product_type_asset_class> product_type_asset_class;
};

struct put_many_product_type_asset_classes_request {
    using response_type = struct put_many_product_type_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.product_type_asset_classes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<product_type_asset_class_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_product_type_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::product_type_asset_class> product_type_asset_classes;
};

struct delete_product_type_asset_class_request {
    using response_type = struct delete_product_type_asset_class_response;
    static constexpr std::string_view nats_subject = "trading.v1.product_type_asset_classes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    product_type_asset_class_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_product_type_asset_class_response {
    ores::utility::domain::result result;
};

struct delete_many_product_type_asset_classes_request {
    using response_type = struct delete_many_product_type_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.product_type_asset_classes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<product_type_asset_class_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_product_type_asset_classes_response {
    ores::utility::domain::result result;
};

struct list_by_product_type_code_product_type_asset_classes_request {
    using response_type = struct list_by_product_type_code_product_type_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.product_type_asset_classes.list_by_product_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string product_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<product_type_asset_classes_filter> filter;
};

struct list_by_product_type_code_product_type_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::product_type_asset_class> product_type_asset_classes;
    std::uint64_t total;
};

}

#endif
