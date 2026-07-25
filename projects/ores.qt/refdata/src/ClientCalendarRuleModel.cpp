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
#include "ores.qt/ClientCalendarRuleModel.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/calendar_rule_protocol.hpp"
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {
std::string calendar_rule_key_extractor(const refdata::domain::calendar_rule& e) {
    return boost::uuids::to_string(e.id);
}
}

ClientCalendarRuleModel::ClientCalendarRuleModel(ClientManager* clientManager, QObject* parent)
    : AbstractClientModel(parent)
    , clientManager_(clientManager)
    , watcher_(new QFutureWatcher<FetchResult>(this))
    , recencyTracker_(calendar_rule_key_extractor)
    , pulseManager_(new RecencyPulseManager(this)) {

    connect(watcher_,
            &QFutureWatcher<FetchResult>::finished,
            this,
            &ClientCalendarRuleModel::onRulesLoaded);

    connect(pulseManager_,
            &RecencyPulseManager::pulse_state_changed,
            this,
            &ClientCalendarRuleModel::onPulseStateChanged);
    connect(pulseManager_,
            &RecencyPulseManager::pulsing_complete,
            this,
            &ClientCalendarRuleModel::onPulsingComplete);
}

int ClientCalendarRuleModel::rowCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return static_cast<int>(calendar_rules_.size());
}

int ClientCalendarRuleModel::columnCount(const QModelIndex& parent) const {
    if (parent.isValid())
        return 0;
    return ColumnCount;
}

QVariant ClientCalendarRuleModel::data(const QModelIndex& index, int role) const {
    if (!index.isValid())
        return {};

    const auto row = static_cast<std::size_t>(index.row());
    if (row >= calendar_rules_.size())
        return {};

    const auto& rule = calendar_rules_[row];

    if (role == Qt::DisplayRole) {
        switch (index.column()) {
            case CalendarCode:
                return QString::fromStdString(rule.calendar_code);
            case Kind:
                return QString::fromStdString(rule.kind);
            case Month:
                return rule.month ? QVariant(static_cast<qlonglong>(*rule.month)) : QVariant();
            case Day:
                return rule.day ? QVariant(static_cast<qlonglong>(*rule.day)) : QVariant();
            case Weekday:
                return rule.weekday ? QVariant(static_cast<qlonglong>(*rule.weekday)) : QVariant();
            case Occurrence:
                return rule.occurrence ? QVariant(static_cast<qlonglong>(*rule.occurrence)) :
                                         QVariant();
            case DayOffset:
                return rule.day_offset ? QVariant(static_cast<qlonglong>(*rule.day_offset)) :
                                         QVariant();
            case Shift:
                return QString::fromStdString(rule.shift);
            case EffectiveFrom:
                return rule.effective_from ?
                           QVariant(static_cast<qlonglong>(*rule.effective_from)) :
                           QVariant();
            case EffectiveTo:
                return rule.effective_to ? QVariant(static_cast<qlonglong>(*rule.effective_to)) :
                                           QVariant();
            case Version:
                return static_cast<qlonglong>(rule.version);
            case ModifiedBy:
                return QString::fromStdString(rule.modified_by);
            default:
                return {};
        }
    }

    if (role == Qt::ForegroundRole) {
        return recency_foreground_color(boost::uuids::to_string(rule.id));
    }

    return {};
}

QVariant
ClientCalendarRuleModel::headerData(int section, Qt::Orientation orientation, int role) const {
    if (orientation != Qt::Horizontal || (role != Qt::DisplayRole && role != Qt::ToolTipRole))
        return {};

    if (role == Qt::ToolTipRole) {
        switch (section) {
            default:
                return {};
        }
    }

    switch (section) {
        case CalendarCode:
            return tr("Calendar");
        case Kind:
            return tr("Kind");
        case Month:
            return tr("Month");
        case Day:
            return tr("Day");
        case Weekday:
            return tr("Weekday");
        case Occurrence:
            return tr("Occurrence");
        case DayOffset:
            return tr("Day Offset");
        case Shift:
            return tr("Shift");
        case EffectiveFrom:
            return tr("From");
        case EffectiveTo:
            return tr("To");
        case Version:
            return tr("Version");
        case ModifiedBy:
            return tr("Modified By");
        default:
            return {};
    }
}

void ClientCalendarRuleModel::refresh() {
    BOOST_LOG_SEV(lg(), debug) << "Calling refresh.";

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring refresh request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot refresh calendar rule model: disconnected.";
        emit loadError("Not connected to server");
        return;
    }

    if (!calendar_rules_.empty()) {
        beginResetModel();
        calendar_rules_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        total_available_count_ = 0;
        endResetModel();
    }

    fetch_calendar_rules(0, page_size_);
}

void ClientCalendarRuleModel::load_page(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "load_page: offset=" << offset << ", limit=" << limit;

    if (is_fetching_) {
        BOOST_LOG_SEV(lg(), warn) << "Fetch already in progress, ignoring load_page request.";
        return;
    }

    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load page: disconnected.";
        return;
    }

    if (!calendar_rules_.empty()) {
        beginResetModel();
        calendar_rules_.clear();
        recencyTracker_.clear();
        pulseManager_->stop_pulsing();
        endResetModel();
    }

    fetch_calendar_rules(offset, limit);
}

void ClientCalendarRuleModel::fetch_calendar_rules(std::uint32_t offset, std::uint32_t limit) {
    is_fetching_ = true;
    QPointer<ClientCalendarRuleModel> self = this;

    QFuture<FetchResult> future = QtConcurrent::run([self, offset, limit]() -> FetchResult {
        return exception_helper::wrap_async_fetch<FetchResult>(
            [&]() -> FetchResult {
                BOOST_LOG_SEV(lg(), debug) << "Making calendar rules request with offset=" << offset
                                           << ", limit=" << limit;
                if (!self || !self->clientManager_) {
                    return {.success = false,
                            .calendar_rules = {},
                            .total_available_count = 0,
                            .error_message = "Model was destroyed",
                            .error_details = {}};
                }

                refdata::messaging::get_calendar_rules_request request;
                request.offset = offset;
                request.limit = limit;

                auto result =
                    self->clientManager_->process_authenticated_request(std::move(request));

                if (!result) {
                    BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << result.error();
                    return {.success = false,
                            .calendar_rules = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result.error()),
                            .error_details = {}};
                }

                // A transport-level success (result is set) does not mean the
                // request itself succeeded -- the server encodes business/
                // repository failures (e.g. a query error) as a normally-
                // deserializable response with success=false and a message,
                // not a transport error. Missing this check silently turns a
                // real backend failure into "0 rows loaded", indistinguishable
                // from a genuinely empty result set.
                if (!result->success) {
                    BOOST_LOG_SEV(lg(), error) << "Server reported failure: " << result->message;
                    return {.success = false,
                            .calendar_rules = {},
                            .total_available_count = 0,
                            .error_message = QString::fromStdString(result->message),
                            .error_details = {}};
                }

                BOOST_LOG_SEV(lg(), debug)
                    << "Fetched " << result->calendar_rules.size()
                    << " calendar rules, total available: " << result->total_available_count;
                return {.success = true,
                        .calendar_rules = std::move(result->calendar_rules),
                        .total_available_count =
                            static_cast<std::uint32_t>(result->total_available_count),
                        .error_message = {},
                        .error_details = {}};
            },
            "calendar rules");
    });

    watcher_->setFuture(future);
}

void ClientCalendarRuleModel::onRulesLoaded() {
    is_fetching_ = false;

    const auto result = watcher_->result();

    if (!result.success) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to fetch calendar rules: " << result.error_message.toStdString();
        emit loadError(result.error_message, result.error_details);
        return;
    }

    total_available_count_ = result.total_available_count;

    const int new_count = static_cast<int>(result.calendar_rules.size());

    if (new_count > 0) {
        beginResetModel();
        calendar_rules_ = std::move(result.calendar_rules);
        endResetModel();

        const bool has_recent = recencyTracker_.update(calendar_rules_);
        if (has_recent && !pulseManager_->is_pulsing()) {
            pulseManager_->start_pulsing();
            BOOST_LOG_SEV(lg(), debug) << "Found " << recencyTracker_.recent_count()
                                       << " calendar rules newer than last reload";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Loaded " << new_count << " calendar rules."
                              << " Total available: " << total_available_count_;

    emit dataLoaded();
}

void ClientCalendarRuleModel::set_page_size(std::uint32_t size) {
    if (size == 0 || size > 1000) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid page size: " << size
                                  << ". Must be between 1 and 1000. Using default: 100";
        page_size_ = 100;
    } else {
        page_size_ = size;
        BOOST_LOG_SEV(lg(), info) << "Page size set to: " << page_size_;
    }
}

const refdata::domain::calendar_rule* ClientCalendarRuleModel::getRule(int row) const {
    const auto idx = static_cast<std::size_t>(row);
    if (idx >= calendar_rules_.size())
        return nullptr;
    return &calendar_rules_[idx];
}


QVariant ClientCalendarRuleModel::recency_foreground_color(const std::string& code) const {
    if (recencyTracker_.is_recent(code) && pulseManager_->is_pulse_on()) {
        return color_constants::stale_indicator;
    }
    return {};
}

void ClientCalendarRuleModel::onPulseStateChanged(bool /*isOn*/) {
    if (!calendar_rules_.empty()) {
        emit dataChanged(
            index(0, 0), index(rowCount() - 1, columnCount() - 1), {Qt::ForegroundRole});
    }
}

void ClientCalendarRuleModel::onPulsingComplete() {
    BOOST_LOG_SEV(lg(), debug) << "Recency highlight pulsing complete";
    recencyTracker_.clear();
}

}
