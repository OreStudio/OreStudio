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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/logger.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/log/attributes/attribute_set.hpp>
#include <boost/log/attributes/constant.hpp>
#include <boost/log/sources/record_ostream.hpp>
#include <boost/log/sources/severity_channel_logger.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <string>

namespace ores::logging {

namespace detail {

struct logger_core {
    explicit logger_core(std::string_view component_name)
        : channel(component_name)
        , backend(boost::log::keywords::channel = std::string_view(channel)) {}

    std::string channel;
    boost::log::sources::severity_channel_logger_mt<detail::boost_severity, std::string_view>
        backend;
};

}

logger::logger() = default;

logger_t make_logger(std::string_view component_name) {
    return logger_t(std::make_shared<detail::logger_core>(component_name));
}

struct scoped_tag::state {
    std::shared_ptr<detail::logger_core> core;
    boost::log::attribute_set::iterator attribute;
};

scoped_tag::scoped_tag(logger_t& logger, std::string_view name, std::string_view value) {
    const auto& core = logger.core();
    if (!core)
        return;
    const auto added = core->backend.add_attribute(
        std::string(name), boost::log::attributes::make_constant(std::string(value)));
    state_ = std::make_shared<state>(state{core, added.first});
}

scoped_tag::~scoped_tag() {
    if (state_)
        state_->core->backend.remove_attribute(state_->attribute);
}

log_record::log_record(logger_t& logger, severity_level severity)
    : logger_(logger)
    , severity_(severity) {}

log_record::~log_record() {
    const auto& core = logger_.core();
    if (!core)
        return;
    try {
        BOOST_LOG_SEV(core->backend, to_boost_severity(severity_)) << text_.str();
    } catch (...) {
        // A logger that throws on its way out of a scope ends the process.
    }
}

}
