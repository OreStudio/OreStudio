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
#ifndef ORES_TRADING_CORE_SERVICE_OPERATIONAL_AUTHORISATION_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_OPERATIONAL_AUTHORISATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_kind.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string_view>

namespace ores::trading::service {

/**
 * @brief What a raise left behind, so the caller can tell the deciders once
 * the transaction has committed.
 */
struct raised_authorisation {
    ores::inbox::domain::approval_kind kind;
    ores::inbox::domain::approval_request request;
};

/**
 * @brief Raises the operational authorisation request of a live trade.
 *
 * Authorisation is metadata about a version, not a state of the trade. This
 * service never writes the trade: it raises one request in the inbox and
 * records which trade version the request covers.
 */
class ORES_TRADING_CORE_EXPORT operational_authorisation_service {
private:
    static constexpr std::string_view logger_name =
        "ores.trading.service.operational_authorisation_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    static constexpr auto kind_code = "trading.operational_authorisation";

    explicit operational_authorisation_service(context ctx);

    /**
     * @brief Raises a request for the trade when it awaits authorisation.
     *
     * An actual trade awaits it once it is live. A
     * draft, a test or a hypothetical trade does not. The write joins the
     * caller's transaction, and the request is not announced here.
     *
     * @return The request raised, or nothing when the trade awaits none.
     */
    std::optional<raised_authorisation> raise_if_awaiting(const boost::uuids::uuid& trade_id);

    /**
     * @brief Tells the deciders that the request waits.
     *
     * Call it after the transaction has committed. A failure to tell is logged
     * and the request stands.
     */
    void announce(const raised_authorisation& raised);

private:
    context ctx_;
};

}

#endif
