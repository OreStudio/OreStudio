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
#include "ores.trading.core/messaging/credit_basket_constituent_registrar.hpp"
#include "ores.trading.core/messaging/credit_default_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/credit_instrument_registrar.hpp"
#include "ores.trading.core/messaging/credit_linked_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/index_credit_default_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/index_credit_default_swap_option_instrument_registrar.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/risk_participation_agreement_instrument_registrar.hpp"
#include "ores.trading.core/messaging/synthetic_cdo_instrument_registrar.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_credit_handlers(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    auto subs = register_credit_instrument_handlers(nats, ctx, verifier);
    auto constituent_subs = register_credit_basket_constituent_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(constituent_subs.begin()),
                std::make_move_iterator(constituent_subs.end()));
    auto credit_default_swap_subs =
        register_credit_default_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(credit_default_swap_subs.begin()),
                std::make_move_iterator(credit_default_swap_subs.end()));
    auto index_credit_default_swap_subs =
        register_index_credit_default_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(index_credit_default_swap_subs.begin()),
                std::make_move_iterator(index_credit_default_swap_subs.end()));
    auto index_credit_default_swap_option_subs =
        register_index_credit_default_swap_option_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(index_credit_default_swap_option_subs.begin()),
                std::make_move_iterator(index_credit_default_swap_option_subs.end()));
    auto synthetic_cdo_subs = register_synthetic_cdo_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(synthetic_cdo_subs.begin()),
                std::make_move_iterator(synthetic_cdo_subs.end()));
    auto credit_linked_swap_subs =
        register_credit_linked_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(credit_linked_swap_subs.begin()),
                std::make_move_iterator(credit_linked_swap_subs.end()));
    auto risk_participation_agreement_subs =
        register_risk_participation_agreement_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(risk_participation_agreement_subs.begin()),
                std::make_move_iterator(risk_participation_agreement_subs.end()));
    return subs;
}

}
