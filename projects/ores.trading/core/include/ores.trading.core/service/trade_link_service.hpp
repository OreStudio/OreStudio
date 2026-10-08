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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_LINK_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_LINK_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_link.hpp"
#include "ores.trading.api/messaging/trade_link_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_link_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade links.
 *
 * Provides a higher-level interface for trade link operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_link_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_link_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_link_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_link_service(context ctx);

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
    messaging::list_trade_links_response
    list_trade_links(const messaging::list_trade_links_request& request);
    messaging::get_trade_link_response
    get_trade_link(const messaging::get_trade_link_request& request);
    messaging::get_many_trade_links_response
    get_many_trade_links(const messaging::get_many_trade_links_request& request);
    messaging::put_trade_link_response
    put_trade_link(const messaging::put_trade_link_request& request);
    messaging::put_many_trade_links_response
    put_many_trade_links(const messaging::put_many_trade_links_request& request);
    messaging::delete_trade_link_response
    delete_trade_link(const messaging::delete_trade_link_request& request);
    messaging::delete_many_trade_links_response
    delete_many_trade_links(const messaging::delete_many_trade_links_request& request);
    messaging::list_trade_link_versions_response
    list_trade_link_versions(const messaging::list_trade_link_versions_request& request);
    messaging::get_trade_link_version_response
    get_trade_link_version(const messaging::get_trade_link_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade links with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade links for the requested page.
     */
    std::vector<domain::trade_link> list_links(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade links.
     *
     * @return Total number of active trade links.
     */
    std::uint32_t count_links();


    /**
     * @brief Retrieves a single trade link as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade link at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_link> get_link_at_version(const std::string& from_trade_id,
                                                          const std::string& to_trade_id,
                                                          const std::string& link_type,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single trade link by its primary key.
     *
     * @return The trade link if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_link> get_link(const std::string& from_trade_id,
                                               const std::string& to_trade_id,
                                               const std::string& link_type);

    /**
     * @brief Retrieves a batch of trade links by primary key.
     */
    std::vector<domain::trade_link> get_links(const std::vector<std::string>& from_trade_ids,
                                              const std::vector<std::string>& to_trade_ids,
                                              const std::vector<std::string>& link_types);

    /**
     * @brief Saves a trade link (creates or updates).
     *
     * @param link The trade link to save.
     * @throws std::exception on failure.
     */
    void save_link(const domain::trade_link& link);

    /**
     * @brief Saves a batch of trade links.
     *
     * @param links The trade links to save.
     * @throws std::exception on failure.
     */
    void save_links(const std::vector<domain::trade_link>& links);

    /**
     * @brief Deletes a trade link by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_link(const std::string& from_trade_id,
                     const std::string& to_trade_id,
                     const std::string& link_type);

    /**
     * @brief Deletes trade links by their primary keys.
     */
    void delete_links(const std::vector<std::string>& from_trade_ids,
                      const std::vector<std::string>& to_trade_ids,
                      const std::vector<std::string>& link_types);

    /**
     * @brief Retrieves all historical versions of a trade link.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::trade_link> get_link_history(const std::string& from_trade_id,
                                                     const std::string& to_trade_id,
                                                     const std::string& link_type);

private:
    context ctx_;
    repository::trade_link_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::trade_link_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::trade_link& out);
};

}

#endif
