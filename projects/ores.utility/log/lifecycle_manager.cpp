/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <boost/make_shared.hpp>
#include <boost/core/null_deleter.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/log/core.hpp>
#include <boost/log/sinks.hpp>
#include <boost/log/common.hpp>
#include <boost/log/attributes.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/sources/logger.hpp>
#include <boost/log/support/date_time.hpp>
#include "ores.utility/log/logging_options_validator.hpp"
#include "ores.utility/log/lifecycle_manager.hpp"

namespace ores::utility::log {

using namespace boost::log;

boost::shared_ptr<lifecycle_manager::file_sink_type>
lifecycle_manager::make_file_sink(
    std::filesystem::path path, const severity_level severity) {

    const std::string extension(".log");
    if (path.extension() != extension)
        path += extension;

    auto backend(boost::make_shared<sinks::text_file_backend>(
            keywords::file_name = path.string(),
            keywords::rotation_size = 300 * 1024 * 1024,
            keywords::time_based_rotation =
            sinks::file::rotation_at_time_point(12, 0, 0)));
    backend->auto_flush(true);

    auto sink = boost::make_shared<file_sink_type>(backend);
    const std::string severity_attr("Severity");
    sink->set_filter(
        expressions::attr<severity_level>(severity_attr) >= severity);

    const std::string channel_attr("Channel");
    const std::string time_stamp_attr("TimeStamp");
    const std::string record_format("%1% [%2%] [%3%] %4%");
    const std::string time_stamp_format("%Y-%m-%d %H:%M:%S.%f");
    sink->set_formatter(expressions::format(record_format)
        % expressions::format_date_time<boost::posix_time::ptime>(
            time_stamp_attr, time_stamp_format)
        % expressions::attr<severity_level>(severity_attr)
        % expressions::attr<std::string>(channel_attr)
        % expressions::smessage);
    return sink;
}

boost::shared_ptr<lifecycle_manager::console_sink_type>
lifecycle_manager::make_console_sink(const severity_level severity) {
    boost::shared_ptr<std::ostream> os(&std::cout, boost::null_deleter());
    auto backend(boost::make_shared<sinks::text_ostream_backend>());
    backend->add_stream(os);

    using sink_type = sinks::synchronous_sink<sinks::text_ostream_backend>;
    auto sink(boost::make_shared<sink_type>(backend));

    const std::string severity_attr("Severity");
    sink->set_filter(
        expressions::attr<severity_level>(severity_attr) >= severity);

    const std::string channel_attr("Channel");
    const std::string time_stamp_attr("TimeStamp");
    const std::string time_stamp_format("%Y-%m-%d %H:%M:%S.%f");
    const std::string record_format("%1% [%2%] [%3%] %4%");
    sink->set_formatter(expressions::format(record_format)
        % expressions::format_date_time<boost::posix_time::ptime>(
            time_stamp_attr, time_stamp_format)
        % expressions::attr<severity_level>(severity_attr)
        % expressions::attr<std::string>(channel_attr)
        % expressions::smessage);

    return sink;
}

lifecycle_manager::lifecycle_manager(std::optional<logging_options> ocfg) {
    /*
     * If no configuration is supplied, logging is to be disabled.
     */
    auto& core(*boost::log::core::get());
    if (!ocfg) {
        core.set_logging_enabled(false);
        return;
    }

    /*
     * A configuration was supplied. Ensure it is valid.
     */
    const auto& cfg(*ocfg);
    logging_options_validator::validate(cfg);
    core.set_logging_enabled(true);

    /*
     * Use the configuration to setup the logging infrastructure for both
     * console and file, if enabled. We don't have to worry about making sure
     * that at least one is enabled - that is the validator's job.
     */
    const auto sl(to_severity_level(cfg.severity));
    if (cfg.output_to_console) {
        console_sink_ = make_console_sink(sl);
        boost::log::core::get()->add_sink(console_sink_);
    }

    if (!cfg.filename.empty()) {
        const auto path(cfg.output_directory / cfg.filename);
        file_sink_ = make_file_sink(path, sl);
        boost::log::core::get()->add_sink(file_sink_);
    }

    /*
     * Finally, add the timestamp attributes.
     */
    const std::string time_stamp_attr("TimeStamp");
    core.add_global_attribute(time_stamp_attr,
        boost::log::attributes::local_clock());
}

lifecycle_manager::~lifecycle_manager() {
    auto core(boost::log::core::get());
    if (file_sink_)
        core->remove_sink(file_sink_);

    if (console_sink_)
        core->remove_sink(console_sink_);
}

}
