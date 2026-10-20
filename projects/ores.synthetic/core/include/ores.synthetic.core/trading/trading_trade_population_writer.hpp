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
#include <map>
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
public:
    /**
     * @param nats A client that acts as the configuration's party. Build it
     *        with the party's own token, because the trading write path reads
     *        the party from the session and refuses a session that names none.
     */
    explicit trading_trade_population_writer(ores::nats::service::nats_client& nats);

    [[nodiscard]] domain::trade_generation_scope
    resolve_scope(const boost::uuids::uuid& config_id) override;

    [[nodiscard]] std::optional<trade_population_stored>
    read_trade(const boost::uuids::uuid& trade_id) override;

    void write_trade(const trade_population_write& write,
                     const domain::trade_generation_scope& scope) override;

    void write_instrument(const trade_population_write& write,
                          const domain::trade_generation_scope& scope) override;

private:
    /**
     * @brief Writes the security's own terms once per run.
     *
     * A security owns its legs, so they are written with the issue row and not
     * with each trade: a second trade of the same ISIN would collide with the
     * first trade's rows, because a leg's parent is the issue.
     */
    void write_issue_leg(const trade_population_write& write,
                         const domain::trade_generation_scope& scope);

    /**
     * @brief Whether the tenant already holds the bond issue.
     *
     * A security belongs to the tenant and not to a config, so another config
     * of the same tenant, or an earlier run, may have written it already.
     */
    bool issue_exists(const boost::uuids::uuid& issue_id);

    /**
     * @brief Issues an entity write, stating the entity on a refusal.
     *
     * @return True when the store took the write.
     *
     * @throws trade_population_write_error on a transport failure.
     */
    template <typename Request>
    bool call_ok(std::string_view subject, const Request& request, const std::string& what);

    ores::nats::service::nats_client& nats_;
    /*
     * The issues this run has written or found already written. A security's
     * terms are one set of rows that its trades read through, so a run writes
     * them once and every later trade of the same security only writes its own
     * instrument.
     */
    std::set<boost::uuids::uuid> issues_written_;
    /*
     * The activity each booked trade names, so the instrument it then receives
     * is stamped with the act that booked it rather than with its own id.
     */
    std::map<boost::uuids::uuid, boost::uuids::uuid> activities_;
};

}

#endif
