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
#include "ores.qt/ReportSchedulerController.hpp"

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ReportSchedulerMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.reporting/messaging/report_definition_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportSchedulerController::ReportSchedulerController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ReportSchedulerController created";
}

void ReportSchedulerController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "report_scheduler");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ReportSchedulerMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ReportSchedulerMdiWindow::statusChanged,
            this, &ReportSchedulerController::statusMessage);
    connect(listWindow_, &ReportSchedulerMdiWindow::errorOccurred,
            this, &ReportSchedulerController::errorMessage);
    connect(listWindow_, &ReportSchedulerMdiWindow::scheduleRequested,
            this, &ReportSchedulerController::onScheduleRequested);
    connect(listWindow_, &ReportSchedulerMdiWindow::unscheduleRequested,
            this, &ReportSchedulerController::onUnscheduleRequested);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Run Reports");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::CalendarClock, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed,
            this, [self = QPointer<ReportSchedulerController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Report Scheduler list window created";
}

void ReportSchedulerController::closeAllWindows() {
    if (listMdiSubWindow_) {
        listMdiSubWindow_->close();
        listMdiSubWindow_ = nullptr;
        listWindow_ = nullptr;
    }
}

void ReportSchedulerController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

EntityListMdiWindow* ReportSchedulerController::listWindow() const {
    return listWindow_;
}

void ReportSchedulerController::onScheduleRequested(
    const std::vector<boost::uuids::uuid>& ids) {

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorMessage(tr("Cannot schedule: not connected to server."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Scheduling " << ids.size() << " report definitions";

    QPointer<ReportSchedulerController> self = this;

    struct ScheduleResult {
        bool success;
        std::string message;
        int count;
    };

    const std::string performed_by = username_.toStdString();

    auto task = [self, ids, performed_by]() -> ScheduleResult {
        if (!self || !self->clientManager_)
            return {false, "Controller destroyed", 0};

        reporting::messaging::schedule_report_definitions_request request;
        request.ids = ids;
        request.performed_by = performed_by;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::schedule_report_definitions_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result)
            return {false, "Failed to communicate with server", 0};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result)
            return {false, "Failed to decompress response", 0};

        auto response = reporting::messaging::schedule_report_definitions_response::
            deserialize(*payload_result);

        if (!response)
            return {false, "Invalid server response", 0};

        return {response->success, response->message, response->scheduled_count};
    };

    auto* watcher = new QFutureWatcher<ScheduleResult>(this);
    connect(watcher, &QFutureWatcher<ScheduleResult>::finished,
            this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Scheduled " << result.count
                                     << " report definitions";
            emit self->statusMessage(tr("Scheduled %1 report definition(s).")
                .arg(result.count));
            if (self->listWindow_)
                self->listWindow_->reload();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Schedule failed: " << result.message;
            const QString msg = QString::fromStdString(result.message);
            emit self->errorMessage(msg);
        }
    });

    QFuture<ScheduleResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void ReportSchedulerController::onUnscheduleRequested(
    const std::vector<boost::uuids::uuid>& ids) {

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorMessage(tr("Cannot unschedule: not connected to server."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Unscheduling " << ids.size() << " report definitions";

    QPointer<ReportSchedulerController> self = this;

    struct UnscheduleResult {
        bool success;
        std::string message;
        int count;
    };

    const std::string performed_by = username_.toStdString();

    auto task = [self, ids, performed_by]() -> UnscheduleResult {
        if (!self || !self->clientManager_)
            return {false, "Controller destroyed", 0};

        reporting::messaging::unschedule_report_definitions_request request;
        request.ids = ids;
        request.performed_by = performed_by;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::unschedule_report_definitions_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result)
            return {false, "Failed to communicate with server", 0};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result)
            return {false, "Failed to decompress response", 0};

        auto response = reporting::messaging::unschedule_report_definitions_response::
            deserialize(*payload_result);

        if (!response)
            return {false, "Invalid server response", 0};

        return {response->success, response->message, response->unscheduled_count};
    };

    auto* watcher = new QFutureWatcher<UnscheduleResult>(this);
    connect(watcher, &QFutureWatcher<UnscheduleResult>::finished,
            this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Unscheduled " << result.count
                                     << " report definitions";
            emit self->statusMessage(tr("Unscheduled %1 report definition(s).")
                .arg(result.count));
            if (self->listWindow_)
                self->listWindow_->reload();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Unschedule failed: " << result.message;
            const QString msg = QString::fromStdString(result.message);
            emit self->errorMessage(msg);
        }
    });

    QFuture<UnscheduleResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
