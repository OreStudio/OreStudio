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
#ifndef ORES_IAM_CLIENT_CLIENT_RUN_TOKEN_MINTER_HPP
#define ORES_IAM_CLIENT_CLIENT_RUN_TOKEN_MINTER_HPP

#include "ores.iam.client/export.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/service/cache/run_token_cache.hpp"

namespace ores::iam::client {

/**
 * @brief Exchanges a run grant for a run token over NATS, with the service's
 * own token.
 *
 * The client must carry the calling service's identity, so build it with
 * @ref make_service_token_provider. The returned minter calls
 * =iam.v1.run_grants.exchange= and answers the token and its expiry, or nothing
 * when IAM refuses or does not answer.
 *
 * @param nats The service's own client; it must outlive the minter.
 */
ORES_IAM_CLIENT_EXPORT ores::service::service::cache::run_token_minter
make_run_token_minter(ores::nats::service::nats_client& nats);

}

#endif
