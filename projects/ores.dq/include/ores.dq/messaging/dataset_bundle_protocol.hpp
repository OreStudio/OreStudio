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
#ifndef ORES_DQ_MESSAGING_DATASET_BUNDLE_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_DATASET_BUNDLE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/dataset_bundle.hpp"

namespace ores::dq::messaging {

// ============================================================================
// Dataset Bundle Messages
// ============================================================================

/**
 * @brief Request to retrieve all dataset bundles.
 */
struct get_dataset_bundles_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_bundles_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_bundles_request& v);

/**
 * @brief Response containing all dataset bundles.
 */
struct get_dataset_bundles_response final {
    std::vector<domain::dataset_bundle> bundles;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_bundles_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_bundles_response& v);

/**
 * @brief Request to save a dataset bundle (create or update).
 */
struct save_dataset_bundle_request final {
    domain::dataset_bundle bundle;

    std::vector<std::byte> serialize() const;
    static std::expected<save_dataset_bundle_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_dataset_bundle_request& v);

/**
 * @brief Response confirming dataset bundle save operation.
 */
struct save_dataset_bundle_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_dataset_bundle_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_dataset_bundle_response& v);

/**
 * @brief Result for a single dataset bundle deletion.
 */
struct delete_dataset_bundle_result final {
    boost::uuids::uuid id;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_result& v);

/**
 * @brief Request to delete one or more dataset bundles.
 */
struct delete_dataset_bundle_request final {
    std::vector<boost::uuids::uuid> ids;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_dataset_bundle_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_request& v);

/**
 * @brief Response confirming dataset bundle deletion(s).
 */
struct delete_dataset_bundle_response final {
    std::vector<delete_dataset_bundle_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_dataset_bundle_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_response& v);

/**
 * @brief Request to retrieve version history for a dataset bundle.
 */
struct get_dataset_bundle_history_request final {
    boost::uuids::uuid id;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_bundle_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_history_request& v);

/**
 * @brief Response containing dataset bundle version history.
 */
struct get_dataset_bundle_history_response final {
    bool success;
    std::string message;
    std::vector<domain::dataset_bundle> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_bundle_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_history_response& v);

}

namespace ores::comms::messaging {

// Dataset Bundle traits
template<>
struct message_traits<dq::messaging::get_dataset_bundles_request> {
    using request_type = dq::messaging::get_dataset_bundles_request;
    using response_type = dq::messaging::get_dataset_bundles_response;
    static constexpr message_type request_message_type =
        message_type::get_dataset_bundles_request;
};

template<>
struct message_traits<dq::messaging::save_dataset_bundle_request> {
    using request_type = dq::messaging::save_dataset_bundle_request;
    using response_type = dq::messaging::save_dataset_bundle_response;
    static constexpr message_type request_message_type =
        message_type::save_dataset_bundle_request;
};

template<>
struct message_traits<dq::messaging::delete_dataset_bundle_request> {
    using request_type = dq::messaging::delete_dataset_bundle_request;
    using response_type = dq::messaging::delete_dataset_bundle_response;
    static constexpr message_type request_message_type =
        message_type::delete_dataset_bundle_request;
};

template<>
struct message_traits<dq::messaging::get_dataset_bundle_history_request> {
    using request_type = dq::messaging::get_dataset_bundle_history_request;
    using response_type = dq::messaging::get_dataset_bundle_history_response;
    static constexpr message_type request_message_type =
        message_type::get_dataset_bundle_history_request;
};

}

#endif
