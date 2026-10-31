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
#ifndef ORES_SYNTHETIC_CORE_TRADING_TRADING_TRADE_POPULATION_WRITER_HPP
#define ORES_SYNTHETIC_CORE_TRADING_TRADING_TRADE_POPULATION_WRITER_HPP

#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/service/trade_population_writer.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include <functional>
#include <map>
#include <optional>
#include <set>
#include <string_view>

namespace ores::synthetic::service {

/**
 * @brief Reaches the trading component through the operations it serves, over
 * an authenticated NATS session.
 *
 * The generator holds no trading repository and no trading storage semantics.
 * A trade is booked by a request to =trading.v1.ops.book_trade=, its
 * instrument by the bond instrument's own write, and its issue and legs by the
 * issue's, so the component that owns the tables decides what a write means.
 *
 * The party is established by the session and not by the request: the trading
 * insert triggers overwrite the instrument's party from
 * =app.current_party_id=, which comes from the caller's token. The client
 * therefore acts as the party the configuration names, and a caller that
 * cannot act as it fails rather than writing against the wrong legal entity.
 */
class ORES_SYNTHETIC_CORE_EXPORT trading_trade_population_writer final
    : public trade_population_writer {
    /**
     * @brief The client that books one trade, in the session of the person the trade is entered as.
     *
     * A booking is written by a person and not by the run: the trade states its author. The caller
     * answers with a client whose session is the slot's trader, so the rows the booking writes name
     * them and not whatever principal carries the generator.
     */
    using booking_client =
        std::function<ores::nats::service::nats_client&(const domain::trade_population_slot& slot)>;

public:
    /**
     * @param nats A client that acts as the configuration's party. Build it
     *        with the party's own token, because the trading write path reads
     *        the party from the session and refuses a session that names none.
     *        It reads the tenant's references and writes the securities.
     * @param booker The client that books each trade, in the session of the
     *        person the slot names. Empty when the caller has one identity for
     *        the whole run, which books as @p nats.
     * @param fallback The person a party is entered by when none of its staff
     *        may trade, which is the run's own principal. A party with one is
     *        logged, because a party whose staff hold no trader is a party whose
     *        trades no person of it owns.
     */
    explicit trading_trade_population_writer(
        ores::nats::service::nats_client& nats,
        booking_client booker = {},
        std::optional<domain::trade_population_trader> fallback = {});

    [[nodiscard]] domain::trade_generation_scope
    resolve_scope(const boost::uuids::uuid& config_id) override;

    [[nodiscard]] std::optional<trade_population_stored>
    read_trade(const boost::uuids::uuid& trade_id) override;

    void write_trade(const domain::trade_population_slot& slot,
                     const domain::trade_generation_scope& scope) override;

    [[nodiscard]] bool structure_exists(const boost::uuids::uuid& structure_id) override;

    void write_structure(const domain::trade_population_structure& structure,
                         const domain::trade_generation_scope& scope) override;

    void write_structure_member(const boost::uuids::uuid& structure_id,
                                const domain::trade_population_slot& slot) override;

    /**
     * @brief The request that books one generated trade.
     *
     * It is built before anything is sent, so the terms the population books
     * under -- its booking nature, its origin and the person it is entered by --
     * are stated in one place that a test can read without a session.
     */
    [[nodiscard]] static ores::trading::messaging::book_trade_request book_request_for(
        const domain::trade_population_slot& slot,
        const domain::trade_generation_scope& scope);

private:
    /**
     * @brief Creates the trade's security with the security operation.
     *
     * It runs before the trade is booked. The operation is create-only and
     * whole or absent, so a security the tenant already holds with equal terms
     * is taken as present.
     */
    void ensure_issue(const domain::trade_population_slot& slot);


    ores::nats::service::nats_client& nats_;
    booking_client booker_;
    std::optional<domain::trade_population_trader> fallback_;
    /*
     * The issues this run has written or found already written. A security's
     * terms are one set of rows that its trades read through, so a run writes
     * them once and every later trade of the same security only writes its own
     * instrument.
     */
    std::set<boost::uuids::uuid> issues_written_;
};

}

#endif
