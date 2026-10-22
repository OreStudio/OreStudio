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
#ifndef ORES_MARKETDATA_CORE_SERVICE_FEED_BINDING_ACTIVITY_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_FEED_BINDING_ACTIVITY_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/feed_binding_activity_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Says what each feed binding of the caller has written.
 *
 * A binding names a source and not a series, and a tick of the tick table carries no source.
 * The ingest loop records the series and the time of the newest tick of each binding in a row
 * of its own, at most every few seconds, and the reader reads that row for each binding.
 *
 * The bindings and the activity rows are isolated by tenant alone, so the reader states the
 * caller's party itself: it reads only that party's bindings and that party's rows. A binding
 * whose source has written nothing for the party stays in the answer with no series and no
 * tick, so the screen can say so.
 */
class ORES_MARKETDATA_CORE_EXPORT feed_binding_activity_reader final {
public:
    /**
     * @brief Reads the activity of each binding the caller can see, in the order of
     * the bindings' source names.
     *
     * @param ctx Database context, with the caller's tenant and party.
     * @return The activity. It fails only when the context names no party, and an empty
     * answer is a success.
     */
    static messaging::list_feed_binding_activity_response read(ores::database::context ctx);

    /// The bindings that belong to @p party_id, from the bindings of a whole tenant.
    static std::vector<domain::feed_binding>
    bindings_of_party(std::vector<domain::feed_binding> bindings,
                      const boost::uuids::uuid& party_id);

    /**
     * @brief The activity of one binding, from the row recorded for it, or from none.
     */
    static messaging::feed_binding_activity
    activity_of(const std::string& source_name,
                const std::optional<repository::feed_binding_activity_record>& recorded);
};

}

#endif
