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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_CORE_SERVICE_FEED_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_FEED_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/feed.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/feed_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing market data feeds.
 *
 * Provides a higher-level interface for market data feed operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT feed_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.feed_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a feed_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit feed_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    ::ores::marketdata::messaging::list_feeds_response
    list_feeds(const ::ores::marketdata::messaging::list_feeds_request& request);
    ::ores::marketdata::messaging::get_feed_response
    get_feed(const ::ores::marketdata::messaging::get_feed_request& request);
    ::ores::marketdata::messaging::get_many_feeds_response
    get_many_feeds(const ::ores::marketdata::messaging::get_many_feeds_request& request);
    ::ores::marketdata::messaging::put_feed_response
    put_feed(const ::ores::marketdata::messaging::put_feed_request& request);
    ::ores::marketdata::messaging::put_many_feeds_response
    put_many_feeds(const ::ores::marketdata::messaging::put_many_feeds_request& request);
    ::ores::marketdata::messaging::delete_feed_response
    delete_feed(const ::ores::marketdata::messaging::delete_feed_request& request);
    ::ores::marketdata::messaging::delete_many_feeds_response
    delete_many_feeds(const ::ores::marketdata::messaging::delete_many_feeds_request& request);
    ::ores::marketdata::messaging::list_feed_versions_response
    list_feed_versions(const ::ores::marketdata::messaging::list_feed_versions_request& request);
    ::ores::marketdata::messaging::get_feed_version_response
    get_feed_version(const ::ores::marketdata::messaging::get_feed_version_request& request);
    /**@}*/

    /**
     * @brief Lists market data feeds with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of market data feeds for the requested page.
     */
    std::vector<domain::feed> list_feeds(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active market data feeds.
     *
     * @return Total number of active market data feeds.
     */
    std::uint32_t count_feeds();


    /**
     * @brief Retrieves a single market data feed as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The market data feed at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::feed> get_feed_at_version(const boost::uuids::uuid& id,
                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single market data feed by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The market data feed if found, std::nullopt otherwise.
     */
    std::optional<domain::feed> get_feed(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single market data feed by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The market data feed if found, std::nullopt otherwise.
     */
    std::optional<domain::feed> get_feed_by_name(const std::string& name);

    /**
     * @brief Retrieves a batch of market data feeds by primary key.
     */
    std::vector<domain::feed> get_feeds(const std::vector<std::string>& ids);

    /**
     * @brief Saves a market data feed (creates or updates).
     *
     * @param feed The market data feed to save.
     * @throws std::exception on failure.
     */
    void save_feed(const domain::feed& feed);

    /**
     * @brief Saves a batch of market data feeds.
     *
     * @param feeds The market data feeds to save.
     * @throws std::exception on failure.
     */
    void save_feeds(const std::vector<domain::feed>& feeds);

    /**
     * @brief Deletes a market data feed by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_feed(const boost::uuids::uuid& id);

    /**
     * @brief Deletes market data feeds by their primary keys.
     */
    void delete_feeds(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a market data feed.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::feed> get_feed_history(const std::string& key);

private:
    context ctx_;
    repository::feed_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::marketdata::messaging::feed_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::feed& out);
};

}

#endif
