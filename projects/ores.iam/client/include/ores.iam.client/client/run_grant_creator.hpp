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
#include "ores.iam.client/client/run_token_minter.hpp"
#ifndef ORES_IAM_CLIENT_CLIENT_RUN_GRANT_CREATOR_HPP
#    define ORES_IAM_CLIENT_CLIENT_RUN_GRANT_CREATOR_HPP

#    include "ores.iam.client/export.hpp"
#    include "ores.nats/service/nats_client.hpp"
#    include <expected>
#    include <string>
#    include <string_view>

namespace ores::iam::client {

/**
 * @brief Gives a person's consent to a resource's runs, over NATS.
 *
 * A run grant is a person's consent, and a service account cannot give it, so
 * the request travels on the person's own token: build @p person_nats with
 * @c with_delegation on the service's client. IAM checks that the person holds
 * every permission of @p role in the party they act for, and answers the active
 * grant when the same person already gave one for @p resource.
 *
 * @param person_nats Carries the person's token.
 * @param resource What the grant serves, as =<component>.<entity>/<id>=.
 * @param role The role a run token carries.
 * @param audience The service roles that may exchange the grant, comma separated.
 *
 * @return The grant id, or the reason IAM refused or did not answer.
 */
ORES_IAM_CLIENT_EXPORT std::expected<std::string, std::string>
create_run_grant(ores::nats::service::nats_client& person_nats,
                 std::string_view resource,
                 std::string_view role,
                 std::string_view audience);

}

#endif
