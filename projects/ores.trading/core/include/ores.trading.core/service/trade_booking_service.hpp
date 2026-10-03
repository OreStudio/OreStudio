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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_BOOKING_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_BOOKING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/messaging/trade_booking_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade bookings.
 *
 * Provides a higher-level interface for trade booking operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_booking_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_booking_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_booking_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_booking_service(context ctx);

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
    messaging::list_trade_bookings_response
    list_trade_bookings(const messaging::list_trade_bookings_request& request);
    messaging::get_trade_booking_response
    get_trade_booking(const messaging::get_trade_booking_request& request);
    messaging::get_many_trade_bookings_response
    get_many_trade_bookings(const messaging::get_many_trade_bookings_request& request);
    messaging::put_trade_booking_response
    put_trade_booking(const messaging::put_trade_booking_request& request);
    messaging::put_many_trade_bookings_response
    put_many_trade_bookings(const messaging::put_many_trade_bookings_request& request);
    messaging::delete_trade_booking_response
    delete_trade_booking(const messaging::delete_trade_booking_request& request);
    messaging::delete_many_trade_bookings_response
    delete_many_trade_bookings(const messaging::delete_many_trade_bookings_request& request);
    messaging::list_trade_booking_versions_response
    list_trade_booking_versions(const messaging::list_trade_booking_versions_request& request);
    messaging::get_trade_booking_version_response
    get_trade_booking_version(const messaging::get_trade_booking_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade bookings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade bookings for the requested page.
     */
    std::vector<domain::trade_booking> list_trade_bookings(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade bookings.
     *
     * @return Total number of active trade bookings.
     */
    std::uint32_t count_trade_bookings();


    /**
     * @brief Retrieves a single trade booking as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade booking at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_booking>
    get_trade_booking_at_version(const boost::uuids::uuid& trade_id, std::uint32_t version);

    /**
     * @brief Retrieves a single trade booking by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade booking if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_booking> get_trade_booking(const boost::uuids::uuid& trade_id);

    /**
     * @brief Retrieves a batch of trade bookings by primary key.
     */
    std::vector<domain::trade_booking>
    get_trade_bookings(const std::vector<std::string>& trade_ids);

    /**
     * @brief Saves a trade booking (creates or updates).
     *
     * @param trade_booking The trade booking to save.
     * @throws std::exception on failure.
     */
    void save_trade_booking(const domain::trade_booking& trade_booking);

    /**
     * @brief Saves a batch of trade bookings.
     *
     * @param trade_bookings The trade bookings to save.
     * @throws std::exception on failure.
     */
    void save_trade_bookings(const std::vector<domain::trade_booking>& trade_bookings);

    /**
     * @brief Deletes a trade booking by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade_booking(const boost::uuids::uuid& trade_id);

    /**
     * @brief Deletes trade bookings by their primary keys.
     */
    void delete_trade_bookings(const std::vector<std::string>& trade_ids);

    /**
     * @brief Retrieves all historical versions of a trade booking.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::trade_booking> get_trade_booking_history(const std::string& trade_id);

private:
    context ctx_;
    repository::trade_booking_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::trade_booking_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::trade_booking& out);
};

}

#endif
