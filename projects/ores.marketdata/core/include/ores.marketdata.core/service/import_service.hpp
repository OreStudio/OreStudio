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
#ifndef ORES_MARKETDATA_CORE_SERVICE_IMPORT_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_IMPORT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.ore.core/market/fx_quote_convention_checker.hpp"
#include <functional>
#include <set>
#include <string>

namespace ores::marketdata::service {

/**
 * @brief Imports ORE market data and fixings files into the database.
 *
 * Parses market.txt and fixings.txt content, names each key and index through
 * the ORE codecs, upserts market series catalog entries, and bulk-inserts
 * observations and fixings into the TimescaleDB hypertables.
 */
class ORES_MARKETDATA_CORE_EXPORT import_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.import_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief The series type fixings are catalogued under.
     *
     * A fixing does not follow ORE's TYPE/METRIC/QUALIFIER/POINT_ID grammar --
     * its key is an index name and a date -- so the import files it under a
     * series type of its own. The export reads this name rather than repeating
     * the string, because the two are inverses and a fixing series the export
     * failed to recognise would be emitted as though it were market data.
     */
    static constexpr std::string_view fixing_series_type = "FIXING";

    /**
     * @brief Supplies the currency pairs the reversed-key correction checks
     *        against, when the caller wants to name them rather than have the
     *        service read them from ores.refdata.
     */
    using known_pairs_provider =
        std::function<std::set<ores::ore::market::fx_quote_convention_checker::currency_pair>()>;

    /**
     * @param auth_nats Authenticated client used to fetch ores.refdata's
     *        currency_pair reference data (for fx_quote_convention_checker).
     *        If the fetch fails (refdata unreachable, etc.), the import
     *        proceeds with no reversed-key correction rather than failing.
     * @param known_pairs When set, the pairs the correction checks against,
     *        and auth_nats is not consulted for them. The reference data is a
     *        live read of another component's state, so a test that supplies
     *        its own pairs is deterministic instead of racing whatever
     *        ores.refdata.service happens to answer.
     */
    import_service(context ctx,
                   ores::nats::service::nats_client& auth_nats,
                   known_pairs_provider known_pairs = {});

    messaging::import_market_data_response import(const messaging::import_market_data_request& req);

private:
    context ctx_;
    ores::nats::service::nats_client& auth_nats_;
    known_pairs_provider known_pairs_;
};

}

#endif
