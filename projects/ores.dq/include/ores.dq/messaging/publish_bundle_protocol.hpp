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
#ifndef ORES_DQ_MESSAGING_PUBLISH_BUNDLE_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_PUBLISH_BUNDLE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/publication_mode.hpp"

namespace ores::dq::messaging {

/**
 * @brief Result for a single dataset within a bundle publication.
 */
struct bundle_dataset_result final {
    std::string dataset_code;
    std::string dataset_name;
    std::string status;  // "success", "failed", "skipped"
    std::uint64_t records_inserted = 0;
    std::uint64_t records_updated = 0;
    std::uint64_t records_skipped = 0;
    std::uint64_t records_deleted = 0;
    std::string error_message;

    std::vector<std::byte> serialize() const;
    static std::expected<bundle_dataset_result, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte>& data);
};

std::ostream& operator<<(std::ostream& s, const bundle_dataset_result& v);

/**
 * @brief Request to publish all datasets in a bundle.
 *
 * This request triggers the bundle publication workflow which:
 * 1. Publishes all datasets in the bundle in display_order
 * 2. Optionally uses atomic mode (all-or-nothing semantics)
 * 3. Records publication history for auditing
 */
struct publish_bundle_request final {
    /**
     * @brief Code of the bundle to publish (e.g., 'base', 'solvaris').
     */
    std::string bundle_code;

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
     * @brief If true, first failure causes entire bundle to rollback.
     *
     * In atomic mode, all datasets succeed or all fail together.
     * Defaults to true for safer behavior.
     */
    bool atomic = true;

    /**
     * @brief JSON parameters for per-dataset configuration.
     *
     * Keyed by artefact_type code. For example:
     * {"lei_parties": {"root_lei": "5493001KJTIIGC8Y1R12"}}
     *
     * Empty string or "{}" means no parameters.
     */
    std::string params_json;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: bundle_code length
     * - N bytes: bundle_code (UTF-8)
     * - 1 byte: mode
     * - 2 bytes: published_by length
     * - N bytes: published_by (UTF-8)
     * - 1 byte: atomic
     * - 2 bytes: params_json length
     * - N bytes: params_json (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<publish_bundle_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const publish_bundle_request& v);

/**
 * @brief Response containing results of bundle publication.
 */
struct publish_bundle_response final {
    /**
     * @brief Overall success flag.
     *
     * In atomic mode, this is true only if all datasets succeeded.
     * In non-atomic mode, this is true if at least one dataset succeeded.
     */
    bool success = false;

    /**
     * @brief Error message if overall publication failed.
     */
    std::string error_message;

    /**
     * @brief Number of datasets processed.
     */
    std::uint32_t datasets_processed = 0;

    /**
     * @brief Number of datasets successfully published.
     */
    std::uint32_t datasets_succeeded = 0;

    /**
     * @brief Number of datasets that failed.
     */
    std::uint32_t datasets_failed = 0;

    /**
     * @brief Number of datasets skipped.
     */
    std::uint32_t datasets_skipped = 0;

    /**
     * @brief Total records inserted across all datasets.
     */
    std::uint64_t total_records_inserted = 0;

    /**
     * @brief Total records updated across all datasets.
     */
    std::uint64_t total_records_updated = 0;

    /**
     * @brief Total records skipped across all datasets.
     */
    std::uint64_t total_records_skipped = 0;

    /**
     * @brief Total records deleted across all datasets.
     */
    std::uint64_t total_records_deleted = 0;

    /**
     * @brief Per-dataset publication results.
     */
    std::vector<bundle_dataset_result> dataset_results;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success
     * - 2 bytes: error_message length
     * - N bytes: error_message (UTF-8)
     * - 4 bytes: datasets_processed
     * - 4 bytes: datasets_succeeded
     * - 4 bytes: datasets_failed
     * - 4 bytes: datasets_skipped
     * - 8 bytes: total_records_inserted
     * - 8 bytes: total_records_updated
     * - 8 bytes: total_records_skipped
     * - 8 bytes: total_records_deleted
     * - 4 bytes: dataset_results count
     * - For each result: (bundle_dataset_result serialization)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<publish_bundle_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const publish_bundle_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits for publish_bundle_request.
 */
template<>
struct message_traits<dq::messaging::publish_bundle_request> {
    using request_type = dq::messaging::publish_bundle_request;
    using response_type = dq::messaging::publish_bundle_response;
    static constexpr message_type request_message_type =
        message_type::publish_bundle_request;
};

}

#endif
