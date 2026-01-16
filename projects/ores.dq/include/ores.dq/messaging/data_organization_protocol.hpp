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
#ifndef ORES_DQ_MESSAGING_DATA_ORGANIZATION_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_DATA_ORGANIZATION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/catalog.hpp"
#include "ores.dq/domain/data_domain.hpp"
#include "ores.dq/domain/subject_area.hpp"

namespace ores::dq::messaging {

// ============================================================================
// Catalog Messages
// ============================================================================

/**
 * @brief Request to retrieve all catalogs.
 */
struct get_catalogs_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_catalogs_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_catalogs_request& v);

/**
 * @brief Response containing all catalogs.
 */
struct get_catalogs_response final {
    std::vector<domain::catalog> catalogs;

    std::vector<std::byte> serialize() const;
    static std::expected<get_catalogs_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_catalogs_response& v);

/**
 * @brief Request to save a catalog (create or update).
 */
struct save_catalog_request final {
    domain::catalog catalog;

    std::vector<std::byte> serialize() const;
    static std::expected<save_catalog_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_catalog_request& v);

/**
 * @brief Response confirming catalog save operation.
 */
struct save_catalog_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_catalog_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_catalog_response& v);

/**
 * @brief Result for a single catalog deletion.
 */
struct delete_catalog_result final {
    std::string name;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_catalog_result& v);

/**
 * @brief Request to delete one or more catalogs.
 */
struct delete_catalog_request final {
    std::vector<std::string> names;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_catalog_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_catalog_request& v);

/**
 * @brief Response confirming catalog deletion(s).
 */
struct delete_catalog_response final {
    std::vector<delete_catalog_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_catalog_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_catalog_response& v);

/**
 * @brief Request to retrieve version history for a catalog.
 */
struct get_catalog_history_request final {
    std::string name;

    std::vector<std::byte> serialize() const;
    static std::expected<get_catalog_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_catalog_history_request& v);

/**
 * @brief Response containing catalog version history.
 */
struct get_catalog_history_response final {
    bool success;
    std::string message;
    std::vector<domain::catalog> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_catalog_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_catalog_history_response& v);

// ============================================================================
// Data Domain Messages
// ============================================================================

/**
 * @brief Request to retrieve all data domains.
 */
struct get_data_domains_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_data_domains_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_data_domains_request& v);

/**
 * @brief Response containing all data domains.
 */
struct get_data_domains_response final {
    std::vector<domain::data_domain> domains;

    std::vector<std::byte> serialize() const;
    static std::expected<get_data_domains_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_data_domains_response& v);

/**
 * @brief Request to save a data domain (create or update).
 */
struct save_data_domain_request final {
    domain::data_domain domain;

    std::vector<std::byte> serialize() const;
    static std::expected<save_data_domain_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_data_domain_request& v);

/**
 * @brief Response confirming data domain save operation.
 */
struct save_data_domain_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_data_domain_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_data_domain_response& v);

/**
 * @brief Result for a single data domain deletion.
 */
struct delete_data_domain_result final {
    std::string name;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_data_domain_result& v);

/**
 * @brief Request to delete one or more data domains.
 */
struct delete_data_domain_request final {
    std::vector<std::string> names;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_data_domain_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_data_domain_request& v);

/**
 * @brief Response confirming data domain deletion(s).
 */
struct delete_data_domain_response final {
    std::vector<delete_data_domain_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_data_domain_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_data_domain_response& v);

/**
 * @brief Request to retrieve version history for a data domain.
 */
struct get_data_domain_history_request final {
    std::string name;

    std::vector<std::byte> serialize() const;
    static std::expected<get_data_domain_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_data_domain_history_request& v);

/**
 * @brief Response containing data domain version history.
 */
struct get_data_domain_history_response final {
    bool success;
    std::string message;
    std::vector<domain::data_domain> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_data_domain_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_data_domain_history_response& v);

// ============================================================================
// Subject Area Messages
// ============================================================================

/**
 * @brief Request to retrieve all subject areas.
 */
struct get_subject_areas_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_subject_areas_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_subject_areas_request& v);

/**
 * @brief Response containing all subject areas.
 */
struct get_subject_areas_response final {
    std::vector<domain::subject_area> subject_areas;

    std::vector<std::byte> serialize() const;
    static std::expected<get_subject_areas_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_subject_areas_response& v);

/**
 * @brief Request to retrieve subject areas for a specific domain.
 */
struct get_subject_areas_by_domain_request final {
    std::string domain_name;

    std::vector<std::byte> serialize() const;
    static std::expected<get_subject_areas_by_domain_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_subject_areas_by_domain_request& v);

/**
 * @brief Response containing subject areas for a domain.
 */
struct get_subject_areas_by_domain_response final {
    std::vector<domain::subject_area> subject_areas;

    std::vector<std::byte> serialize() const;
    static std::expected<get_subject_areas_by_domain_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_subject_areas_by_domain_response& v);

/**
 * @brief Request to save a subject area (create or update).
 */
struct save_subject_area_request final {
    domain::subject_area subject_area;

    std::vector<std::byte> serialize() const;
    static std::expected<save_subject_area_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_subject_area_request& v);

/**
 * @brief Response confirming subject area save operation.
 */
struct save_subject_area_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_subject_area_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_subject_area_response& v);

/**
 * @brief Key for a subject area (composite key: name + domain_name).
 */
struct subject_area_key final {
    std::string name;
    std::string domain_name;
};

std::ostream& operator<<(std::ostream& s, const subject_area_key& v);

/**
 * @brief Result for a single subject area deletion.
 */
struct delete_subject_area_result final {
    subject_area_key key;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_subject_area_result& v);

/**
 * @brief Request to delete one or more subject areas.
 */
struct delete_subject_area_request final {
    std::vector<subject_area_key> keys;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_subject_area_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_subject_area_request& v);

/**
 * @brief Response confirming subject area deletion(s).
 */
struct delete_subject_area_response final {
    std::vector<delete_subject_area_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_subject_area_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_subject_area_response& v);

/**
 * @brief Request to retrieve version history for a subject area.
 */
struct get_subject_area_history_request final {
    subject_area_key key;

    std::vector<std::byte> serialize() const;
    static std::expected<get_subject_area_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_subject_area_history_request& v);

/**
 * @brief Response containing subject area version history.
 */
struct get_subject_area_history_response final {
    bool success;
    std::string message;
    std::vector<domain::subject_area> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_subject_area_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_subject_area_history_response& v);

}

namespace ores::comms::messaging {

// Catalog traits
template<>
struct message_traits<dq::messaging::get_catalogs_request> {
    using request_type = dq::messaging::get_catalogs_request;
    using response_type = dq::messaging::get_catalogs_response;
    static constexpr message_type request_message_type =
        message_type::get_catalogs_request;
};

template<>
struct message_traits<dq::messaging::save_catalog_request> {
    using request_type = dq::messaging::save_catalog_request;
    using response_type = dq::messaging::save_catalog_response;
    static constexpr message_type request_message_type =
        message_type::save_catalog_request;
};

template<>
struct message_traits<dq::messaging::delete_catalog_request> {
    using request_type = dq::messaging::delete_catalog_request;
    using response_type = dq::messaging::delete_catalog_response;
    static constexpr message_type request_message_type =
        message_type::delete_catalog_request;
};

template<>
struct message_traits<dq::messaging::get_catalog_history_request> {
    using request_type = dq::messaging::get_catalog_history_request;
    using response_type = dq::messaging::get_catalog_history_response;
    static constexpr message_type request_message_type =
        message_type::get_catalog_history_request;
};

// Data Domain traits
template<>
struct message_traits<dq::messaging::get_data_domains_request> {
    using request_type = dq::messaging::get_data_domains_request;
    using response_type = dq::messaging::get_data_domains_response;
    static constexpr message_type request_message_type =
        message_type::get_data_domains_request;
};

template<>
struct message_traits<dq::messaging::save_data_domain_request> {
    using request_type = dq::messaging::save_data_domain_request;
    using response_type = dq::messaging::save_data_domain_response;
    static constexpr message_type request_message_type =
        message_type::save_data_domain_request;
};

template<>
struct message_traits<dq::messaging::delete_data_domain_request> {
    using request_type = dq::messaging::delete_data_domain_request;
    using response_type = dq::messaging::delete_data_domain_response;
    static constexpr message_type request_message_type =
        message_type::delete_data_domain_request;
};

template<>
struct message_traits<dq::messaging::get_data_domain_history_request> {
    using request_type = dq::messaging::get_data_domain_history_request;
    using response_type = dq::messaging::get_data_domain_history_response;
    static constexpr message_type request_message_type =
        message_type::get_data_domain_history_request;
};

// Subject Area traits
template<>
struct message_traits<dq::messaging::get_subject_areas_request> {
    using request_type = dq::messaging::get_subject_areas_request;
    using response_type = dq::messaging::get_subject_areas_response;
    static constexpr message_type request_message_type =
        message_type::get_subject_areas_request;
};

template<>
struct message_traits<dq::messaging::get_subject_areas_by_domain_request> {
    using request_type = dq::messaging::get_subject_areas_by_domain_request;
    using response_type = dq::messaging::get_subject_areas_by_domain_response;
    static constexpr message_type request_message_type =
        message_type::get_subject_areas_by_domain_request;
};

template<>
struct message_traits<dq::messaging::save_subject_area_request> {
    using request_type = dq::messaging::save_subject_area_request;
    using response_type = dq::messaging::save_subject_area_response;
    static constexpr message_type request_message_type =
        message_type::save_subject_area_request;
};

template<>
struct message_traits<dq::messaging::delete_subject_area_request> {
    using request_type = dq::messaging::delete_subject_area_request;
    using response_type = dq::messaging::delete_subject_area_response;
    static constexpr message_type request_message_type =
        message_type::delete_subject_area_request;
};

template<>
struct message_traits<dq::messaging::get_subject_area_history_request> {
    using request_type = dq::messaging::get_subject_area_history_request;
    using response_type = dq::messaging::get_subject_area_history_response;
    static constexpr message_type request_message_type =
        message_type::get_subject_area_history_request;
};

}

#endif
