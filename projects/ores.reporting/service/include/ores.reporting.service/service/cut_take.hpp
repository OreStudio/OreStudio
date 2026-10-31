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
#ifndef ORES_REPORTING_SERVICE_SERVICE_CUT_TAKE_HPP
#define ORES_REPORTING_SERVICE_SERVICE_CUT_TAKE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/service/cut_body_reader.hpp"
#include "ores.reporting.service/export.hpp"
#include <string>

namespace ores::reporting::service {

/**
 * @brief Assembles the body of a cut by hand, and takes it.
 *
 * The take is one act and not three: the body, the approval request it raises
 * and the cut's state land together or not at all, so a take that wrote a body
 * and then asked for a signature cannot leave a cut nobody is asked to sign.
 *
 * The assembly reads the parts the store can answer at the instant through the
 * reader the caller supplies, records every part it could not fill as a gap with
 * the reason, and answers the gaps rather than failing the whole.
 *
 * A gap stops the press. The design refuses a body that does not meet the
 * definition before it is signed, so a take that carries a gap writes the
 * attempt as an incomplete body, raises no request and says what closes each
 * gap.
 */
class ORES_REPORTING_SERVICE_EXPORT cut_take_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.cut_take_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @param ctx The caller's context: the take acts as the person it carries.
     * @param reader Answers what the store holds for each part of the body.
     */
    cut_take_service(context ctx, cut_body_reader& reader);

    /**
     * @brief Assembles the body and, unless the call previews, takes it.
     *
     * A call that answers a refusal writes nothing and states the sentence.
     */
    messaging::take_cut_response take(const messaging::take_cut_request& request);

private:
    context ctx_;
    cut_body_reader& reader_;
};

}

#endif
