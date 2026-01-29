/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_MESSAGING_DATASET_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_DATASET_PROTOCOL_HPP

#include <cstddef>
#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/dataset.hpp"
#include "ores.dq/domain/methodology.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace ores::dq::messaging {

// ============================================================================
// Dataset Serialization Helpers
// ============================================================================

/**
 * @brief Serialize a dataset to binary format.
 *
 * These helpers are exposed for reuse by other protocol implementations that
 * need to serialize/deserialize datasets (e.g., publication_protocol).
 */
void write_dataset(std::vector<std::byte>& buffer, const domain::dataset& d);

/**
 * @brief Deserialize a dataset from binary format.
 */
std::expected<domain::dataset, ores::utility::serialization::error_code>
read_dataset(std::span<const std::byte>& data);

// ============================================================================
// Dataset Messages
// ============================================================================

/**
 * @brief Request to retrieve all datasets.
 */
struct get_datasets_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_datasets_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_datasets_request& v);

/**
 * @brief Response containing all datasets.
 */
struct get_datasets_response final {
    std::vector<domain::dataset> datasets;

    std::vector<std::byte> serialize() const;
    static std::expected<get_datasets_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_datasets_response& v);

/**
 * @brief Request to save a dataset (create or update).
 */
struct save_dataset_request final {
    domain::dataset dataset;

    std::vector<std::byte> serialize() const;
    static std::expected<save_dataset_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_dataset_request& v);

/**
 * @brief Response confirming dataset save operation.
 */
struct save_dataset_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_dataset_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_dataset_response& v);

/**
 * @brief Result for a single dataset deletion.
 */
struct delete_dataset_result final {
    boost::uuids::uuid id;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_result& v);

/**
 * @brief Request to delete one or more datasets.
 */
struct delete_dataset_request final {
    std::vector<boost::uuids::uuid> ids;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_dataset_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_request& v);

/**
 * @brief Response confirming dataset deletion(s).
 */
struct delete_dataset_response final {
    std::vector<delete_dataset_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_dataset_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_response& v);

/**
 * @brief Request to retrieve version history for a dataset.
 */
struct get_dataset_history_request final {
    boost::uuids::uuid id;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_history_request& v);

/**
 * @brief Response containing dataset version history.
 */
struct get_dataset_history_response final {
    bool success;
    std::string message;
    std::vector<domain::dataset> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_history_response& v);

// ============================================================================
// Methodology Messages
// ============================================================================

/**
 * @brief Request to retrieve all methodologies.
 */
struct get_methodologies_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_methodologies_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_methodologies_request& v);

/**
 * @brief Response containing all methodologies.
 */
struct get_methodologies_response final {
    std::vector<domain::methodology> methodologies;

    std::vector<std::byte> serialize() const;
    static std::expected<get_methodologies_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_methodologies_response& v);

/**
 * @brief Request to save a methodology (create or update).
 */
struct save_methodology_request final {
    domain::methodology methodology;

    std::vector<std::byte> serialize() const;
    static std::expected<save_methodology_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_methodology_request& v);

/**
 * @brief Response confirming methodology save operation.
 */
struct save_methodology_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_methodology_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_methodology_response& v);

/**
 * @brief Result for a single methodology deletion.
 */
struct delete_methodology_result final {
    boost::uuids::uuid id;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_methodology_result& v);

/**
 * @brief Request to delete one or more methodologies.
 */
struct delete_methodology_request final {
    std::vector<boost::uuids::uuid> ids;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_methodology_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_methodology_request& v);

/**
 * @brief Response confirming methodology deletion(s).
 */
struct delete_methodology_response final {
    std::vector<delete_methodology_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_methodology_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_methodology_response& v);

/**
 * @brief Request to retrieve version history for a methodology.
 */
struct get_methodology_history_request final {
    boost::uuids::uuid id;

    std::vector<std::byte> serialize() const;
    static std::expected<get_methodology_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_methodology_history_request& v);

/**
 * @brief Response containing methodology version history.
 */
struct get_methodology_history_response final {
    bool success;
    std::string message;
    std::vector<domain::methodology> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_methodology_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_methodology_history_response& v);

}

namespace ores::comms::messaging {

// Dataset traits
template<>
struct message_traits<dq::messaging::get_datasets_request> {
    using request_type = dq::messaging::get_datasets_request;
    using response_type = dq::messaging::get_datasets_response;
    static constexpr message_type request_message_type =
        message_type::get_datasets_request;
};

template<>
struct message_traits<dq::messaging::save_dataset_request> {
    using request_type = dq::messaging::save_dataset_request;
    using response_type = dq::messaging::save_dataset_response;
    static constexpr message_type request_message_type =
        message_type::save_dataset_request;
};

template<>
struct message_traits<dq::messaging::delete_dataset_request> {
    using request_type = dq::messaging::delete_dataset_request;
    using response_type = dq::messaging::delete_dataset_response;
    static constexpr message_type request_message_type =
        message_type::delete_dataset_request;
};

template<>
struct message_traits<dq::messaging::get_dataset_history_request> {
    using request_type = dq::messaging::get_dataset_history_request;
    using response_type = dq::messaging::get_dataset_history_response;
    static constexpr message_type request_message_type =
        message_type::get_dataset_history_request;
};

// Methodology traits
template<>
struct message_traits<dq::messaging::get_methodologies_request> {
    using request_type = dq::messaging::get_methodologies_request;
    using response_type = dq::messaging::get_methodologies_response;
    static constexpr message_type request_message_type =
        message_type::get_methodologies_request;
};

template<>
struct message_traits<dq::messaging::save_methodology_request> {
    using request_type = dq::messaging::save_methodology_request;
    using response_type = dq::messaging::save_methodology_response;
    static constexpr message_type request_message_type =
        message_type::save_methodology_request;
};

template<>
struct message_traits<dq::messaging::delete_methodology_request> {
    using request_type = dq::messaging::delete_methodology_request;
    using response_type = dq::messaging::delete_methodology_response;
    static constexpr message_type request_message_type =
        message_type::delete_methodology_request;
};

template<>
struct message_traits<dq::messaging::get_methodology_history_request> {
    using request_type = dq::messaging::get_methodology_history_request;
    using response_type = dq::messaging::get_methodology_history_response;
    static constexpr message_type request_message_type =
        message_type::get_methodology_history_request;
};

}

#endif
