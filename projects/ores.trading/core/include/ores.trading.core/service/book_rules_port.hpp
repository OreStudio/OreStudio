/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 *
 */
#ifndef ORES_TRADING_CORE_SERVICE_BOOK_RULES_PORT_HPP
#define ORES_TRADING_CORE_SERVICE_BOOK_RULES_PORT_HPP

#include <boost/uuid/uuid.hpp>
#include <string_view>

namespace ores::trading::service {

/**
 * @brief What a book's own rules say about a booking, as another service knows
 * them.
 *
 * A book's status and the product types it allows are reference data: the
 * refdata service owns both and this component stores neither. A booking must
 * land in a book that is open, under a product type the book allows, and both
 * are rules about the book, so asking is the only honest way to know them.
 *
 * The interface is a port because the caller decides how to ask: the service
 * process asks over the message bus, and a test hands in its own answer. The
 * booking service is a database service with no bus of its own, and it must
 * stay one.
 */
class book_rules_port {
public:
    /** What the port can say about a book's status. */
    enum class status {
        open,
        not_open,
        unknown,
    };

    /** What the port can say about the book's view of a product type. */
    enum class product_type {
        allows,
        refuses,
        unknown,
    };

    /** One book's answer to one booking. */
    struct answer {
        status book_status = status::unknown;
        product_type allowed = product_type::unknown;
    };

    virtual ~book_rules_port() = default;

    /**
     * @brief What the book a booking names says about it, asked as
     * @p actor_token.
     *
     * `unknown` means the answer could not be had: the book was not found, or
     * the ask was refused, or the other service did not answer. A caller that
     * cannot tell a frozen book from an unreadable one must not pretend it is
     * open, and it must not pretend a rule was satisfied, so the booking
     * service refuses on `not_open` and on `refuses`, and carries on with an
     * `unknown`, which it logs. A booking refused because refdata was busy
     * would be a worse fault than the one these rules prevent.
     */
    [[nodiscard]] virtual answer of(const boost::uuids::uuid& book_id,
                                    std::string_view product_type_code,
                                    std::string_view actor_token) const = 0;
};

/** A port that knows nothing, for callers that cannot ask. */
class unknown_book_rules final : public book_rules_port {
public:
    [[nodiscard]] answer
    of(const boost::uuids::uuid&, std::string_view, std::string_view) const override {
        return {};
    }
};

}

#endif
