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
#ifndef ORES_DQ_MESSAGING_PUBLICATION_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_PUBLICATION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/dataset.hpp"
#include "ores.dq/domain/publication.hpp"
#include "ores.dq/domain/publication_mode.hpp"
#include "ores.dq/domain/publication_result.hpp"

namespace ores::dq::messaging {

// ============================================================================
// Publication Messages
// ============================================================================

/**
 * @brief Request to publish one or more datasets to production tables.
 *
 * This triggers the publication workflow:
 * 1. Resolve dataset dependencies (if resolve_dependencies is true)
 * 2. Determine publication order (dependencies first)
 * 3. Call appropriate dq_populate_* functions for each dataset
 * 4. Record publication history
 */
struct publish_datasets_request final {
    /**
     * @brief IDs of datasets to publish.
     */
    std::vector<boost::uuids::uuid> dataset_ids;

    /**
     * @brief Publication mode.
     *
     * Controls how existing data is handled:
     * - upsert: Insert new, update existing
     * - insert_only: Insert new, skip existing
     * - replace_all: Delete all, then insert
     */
    domain::publication_mode mode = domain::publication_mode::upsert;

    /**
     * @brief Username of person initiating publication.
     */
    std::string published_by;

    /**
     * @brief If true, automatically include and publish dependencies first.
     */
    bool resolve_dependencies = true;

    /**
     * @brief If true, first failure causes entire publication to abort.
     *
     * In atomic mode, all datasets succeed or all fail together.
     * Defaults to true for safer behavior.
     */
    bool atomic = true;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 4 bytes: dataset_ids count
     * - N * 16 bytes: dataset_ids (UUIDs)
     * - 1 byte: mode
     * - 2 bytes: published_by length
     * - N bytes: published_by (UTF-8)
     * - 1 byte: resolve_dependencies
     * - 1 byte: atomic
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<publish_datasets_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const publish_datasets_request& v);

/**
 * @brief Response containing results of dataset publication.
 */
struct publish_datasets_response final {
    /**
     * @brief Results for each dataset published.
     *
     * Includes both requested datasets and their dependencies (if resolved).
     * Results are in publication order (dependencies first).
     */
    std::vector<domain::publication_result> results;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: results count
     * - For each result:
     *   - 16 bytes: dataset_id (UUID)
     *   - 2 bytes: dataset_code length
     *   - N bytes: dataset_code (UTF-8)
     *   - 2 bytes: dataset_name length
     *   - N bytes: dataset_name (UTF-8)
     *   - 2 bytes: target_table length
     *   - N bytes: target_table (UTF-8)
     *   - 8 bytes: records_inserted
     *   - 8 bytes: records_updated
     *   - 8 bytes: records_skipped
     *   - 8 bytes: records_deleted
     *   - 1 byte: success
     *   - 2 bytes: error_message length
     *   - N bytes: error_message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<publish_datasets_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const publish_datasets_response& v);

// ============================================================================
// Publication History Messages
// ============================================================================

/**
 * @brief Request to get publication history.
 *
 * Can optionally filter by dataset_id. If dataset_id is nil, returns
 * recent publications across all datasets.
 */
struct get_publications_request final {
    /**
     * @brief Optional dataset ID to filter by.
     *
     * If nil (all zeros), returns recent publications across all datasets.
     */
    boost::uuids::uuid dataset_id;

    /**
     * @brief Maximum number of records to return.
     */
    std::uint32_t limit = 100;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 16 bytes: dataset_id (UUID)
     * - 4 bytes: limit
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<get_publications_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_publications_request& v);

/**
 * @brief Response containing publication history.
 */
struct get_publications_response final {
    /**
     * @brief Publication records, newest first.
     */
    std::vector<domain::publication> publications;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: publications count
     * - For each publication:
     *   - 16 bytes: id (UUID)
     *   - 16 bytes: dataset_id (UUID)
     *   - 2 bytes: dataset_code length
     *   - N bytes: dataset_code (UTF-8)
     *   - 1 byte: mode
     *   - 2 bytes: target_table length
     *   - N bytes: target_table (UTF-8)
     *   - 8 bytes: records_inserted
     *   - 8 bytes: records_updated
     *   - 8 bytes: records_skipped
     *   - 8 bytes: records_deleted
     *   - 2 bytes: published_by length
     *   - N bytes: published_by (UTF-8)
     *   - 8 bytes: published_at (milliseconds since epoch)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<get_publications_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_publications_response& v);

// ============================================================================
// Dependency Resolution Messages
// ============================================================================

/**
 * @brief Request to resolve dependencies for datasets before publishing.
 *
 * This allows clients to preview which datasets will be published and in
 * what order, including any dependencies that will be automatically included.
 * The response returns the full ordered list of datasets.
 */
struct resolve_dependencies_request final {
    /**
     * @brief IDs of datasets to resolve dependencies for.
     */
    std::vector<boost::uuids::uuid> dataset_ids;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 4 bytes: dataset_ids count
     * - N * 16 bytes: dataset_ids (UUIDs)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<resolve_dependencies_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const resolve_dependencies_request& v);

/**
 * @brief Response containing the resolved publication order.
 *
 * Returns datasets in the order they should be published, with dependencies
 * appearing before the datasets that depend on them.
 */
struct resolve_dependencies_response final {
    /**
     * @brief Datasets in publication order.
     *
     * Dependencies appear first, followed by datasets that depend on them.
     * Includes both explicitly requested datasets and their dependencies.
     */
    std::vector<domain::dataset> datasets;

    /**
     * @brief IDs of datasets that were explicitly requested (not dependencies).
     *
     * This allows the client to distinguish between requested datasets and
     * automatically included dependencies in the UI.
     */
    std::vector<boost::uuids::uuid> requested_ids;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 4 bytes: datasets count
     * - For each dataset: (full dataset serialization)
     * - 4 bytes: requested_ids count
     * - N * 16 bytes: requested_ids (UUIDs)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<resolve_dependencies_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const resolve_dependencies_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits for publish_datasets_request.
 */
template<>
struct message_traits<dq::messaging::publish_datasets_request> {
    using request_type = dq::messaging::publish_datasets_request;
    using response_type = dq::messaging::publish_datasets_response;
    static constexpr message_type request_message_type =
        message_type::publish_datasets_request;
};

/**
 * @brief Message traits for get_publications_request.
 */
template<>
struct message_traits<dq::messaging::get_publications_request> {
    using request_type = dq::messaging::get_publications_request;
    using response_type = dq::messaging::get_publications_response;
    static constexpr message_type request_message_type =
        message_type::get_publications_request;
};

/**
 * @brief Message traits for resolve_dependencies_request.
 */
template<>
struct message_traits<dq::messaging::resolve_dependencies_request> {
    using request_type = dq::messaging::resolve_dependencies_request;
    using response_type = dq::messaging::resolve_dependencies_response;
    static constexpr message_type request_message_type =
        message_type::resolve_dependencies_request;
};

}

#endif
