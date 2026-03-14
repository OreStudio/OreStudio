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
#include "ores.qt/ReportDefinitionController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ReportDefinitionMdiWindow.hpp"
#include "ores.qt/ReportDefinitionDetailDialog.hpp"
#include "ores.qt/ReportDefinitionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.reporting/messaging/report_definition_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {

constexpr std::string_view report_definition_event_name =
    "ores.reporting.report_definition_changed";

} // namespace

ReportDefinitionController::ReportDefinitionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      changeReasonCache_(changeReasonCache),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "ReportDefinitionController created";

    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &ReportDefinitionController::onNotificationReceived);

        auto subscribeAll = [self = QPointer<ReportDefinitionController>(this)]() {
            if (!self) return;
            BOOST_LOG_SEV(lg(), info) << "Subscribing to report definition change events";
            self->clientManager_->subscribeToEvent(std::string{report_definition_event_name});
        };

        connect(clientManager_, &ClientManager::loggedIn, this, subscribeAll);
        connect(clientManager_, &ClientManager::reconnected, this, subscribeAll);

        if (clientManager_->isConnected())
            subscribeAll();
    }
}

ReportDefinitionController::~ReportDefinitionController() {
    BOOST_LOG_SEV(lg(), debug) << "ReportDefinitionController destroyed";
    if (clientManager_)
        clientManager_->unsubscribeFromEvent(std::string{report_definition_event_name});
}

void ReportDefinitionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "report_definitions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ReportDefinitionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ReportDefinitionMdiWindow::statusChanged,
            this, &ReportDefinitionController::statusMessage);
    connect(listWindow_, &ReportDefinitionMdiWindow::errorOccurred,
            this, &ReportDefinitionController::errorMessage);
    connect(listWindow_, &ReportDefinitionMdiWindow::showDefinitionDetails,
            this, &ReportDefinitionController::onShowDetails);
    connect(listWindow_, &ReportDefinitionMdiWindow::addNewRequested,
            this, &ReportDefinitionController::onAddNewRequested);
    connect(listWindow_, &ReportDefinitionMdiWindow::showDefinitionHistory,
            this, &ReportDefinitionController::onShowHistory);
    connect(listWindow_, &ReportDefinitionMdiWindow::scheduleRequested,
            this, &ReportDefinitionController::onScheduleRequested);
    connect(listWindow_, &ReportDefinitionMdiWindow::unscheduleRequested,
            this, &ReportDefinitionController::onUnscheduleRequested);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Report Definitions");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentTable, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<ReportDefinitionController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Report Definition list window created";
}

void ReportDefinitionController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    // Close all managed windows
    QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key)) {
            window->close();
        }
    }
    managed_windows_.clear();

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void ReportDefinitionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void ReportDefinitionController::onShowDetails(
    const reporting::domain::report_definition& definition) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << definition.name;
    showDetailWindow(definition);
}

void ReportDefinitionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new report definition requested";
    showAddWindow();
}

void ReportDefinitionController::onShowHistory(
    const reporting::domain::report_definition& definition) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << definition.name;
    showHistoryWindow(definition);
}

void ReportDefinitionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new report definition";

    auto* detailDialog = new ReportDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setCreateMode(true);

    connect(detailDialog, &ReportDefinitionDetailDialog::statusMessage,
            this, &ReportDefinitionController::statusMessage);
    connect(detailDialog, &ReportDefinitionDetailDialog::errorMessage,
            this, &ReportDefinitionController::errorMessage);
    connect(detailDialog, &ReportDefinitionDetailDialog::definitionSaved,
            this, [self = QPointer<ReportDefinitionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Definition saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Report Definition");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentTable, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ReportDefinitionController::showDetailWindow(
    const reporting::domain::report_definition& definition) {

    const QString identifier = QString::fromStdString(definition.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << definition.name;

    auto* detailDialog = new ReportDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setCreateMode(false);
    detailDialog->setDefinition(definition);

    connect(detailDialog, &ReportDefinitionDetailDialog::statusMessage,
            this, &ReportDefinitionController::statusMessage);
    connect(detailDialog, &ReportDefinitionDetailDialog::errorMessage,
            this, &ReportDefinitionController::errorMessage);
    connect(detailDialog, &ReportDefinitionDetailDialog::definitionSaved,
            this, [self = QPointer<ReportDefinitionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Definition saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &ReportDefinitionDetailDialog::definitionDeleted,
            this, [self = QPointer<ReportDefinitionController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Definition deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Report Definition: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentTable, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ReportDefinitionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ReportDefinitionController::showHistoryWindow(
    const reporting::domain::report_definition& definition) {
    const QString code = QString::fromStdString(definition.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for report definition: "
                              << definition.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << definition.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << definition.name;

    auto* historyDialog = new ReportDefinitionHistoryDialog(
        definition.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &ReportDefinitionHistoryDialog::statusChanged,
            this, [self = QPointer<ReportDefinitionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &ReportDefinitionHistoryDialog::errorOccurred,
            this, [self = QPointer<ReportDefinitionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &ReportDefinitionHistoryDialog::revertVersionRequested,
            this, &ReportDefinitionController::onRevertVersion);
    connect(historyDialog, &ReportDefinitionHistoryDialog::openVersionRequested,
            this, &ReportDefinitionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Report Definition History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<ReportDefinitionController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ReportDefinitionController::onOpenVersion(
    const reporting::domain::report_definition& definition, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for report definition: " << definition.name;

    const QString code = QString::fromStdString(definition.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new ReportDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDefinition(definition);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ReportDefinitionDetailDialog::statusMessage,
            this, [self = QPointer<ReportDefinitionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &ReportDefinitionDetailDialog::errorMessage,
            this, [self = QPointer<ReportDefinitionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Report Definition: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<ReportDefinitionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ReportDefinitionController::onRevertVersion(
    const reporting::domain::report_definition& definition) {
    BOOST_LOG_SEV(lg(), info) << "Reverting report definition to version: "
                              << definition.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new ReportDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDefinition(definition);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ReportDefinitionDetailDialog::statusMessage,
            this, &ReportDefinitionController::statusMessage);
    connect(detailDialog, &ReportDefinitionDetailDialog::errorMessage,
            this, &ReportDefinitionController::errorMessage);
    connect(detailDialog, &ReportDefinitionDetailDialog::definitionSaved,
            this, [self = QPointer<ReportDefinitionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Report Definition reverted: " << code.toStdString();
        emit self->statusMessage(QString("Report Definition '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Report Definition: %1")
        .arg(QString::fromStdString(definition.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* ReportDefinitionController::listWindow() const {
    return listWindow_;
}

void ReportDefinitionController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds, const QString& /*tenantId*/) {

    if (eventType.toStdString() != report_definition_event_name)
        return;

    BOOST_LOG_SEV(lg(), info) << "Received report_definition_changed notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " id(s)";

    if (listWindow_)
        listWindow_->markAsStale();
}

void ReportDefinitionController::onScheduleRequested(
    const std::vector<boost::uuids::uuid>& ids) {

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorMessage(tr("Cannot schedule: not connected to server."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Scheduling " << ids.size() << " report definitions";

    QPointer<ReportDefinitionController> self = this;
    const std::string performed_by = username_.toStdString();

    struct ScheduleResult { bool success; std::string message; int count; };

    auto task = [self, ids, performed_by]() -> ScheduleResult {
        if (!self || !self->clientManager_)
            return {false, "Controller destroyed", 0};

        reporting::messaging::schedule_report_definitions_request request;
        for (const auto& id : ids) {
            request.ids.push_back(boost::uuids::to_string(id));
        }
        request.performed_by = performed_by;
        auto response_result = self->clientManager_->process_authenticated_request(std::move(request));
        if (!response_result)
            return {false, "Failed to communicate with server", 0};

        return {response_result->success, response_result->message, response_result->scheduled_count};
    };

    auto* watcher = new QFutureWatcher<ScheduleResult>(this);
    connect(watcher, &QFutureWatcher<ScheduleResult>::finished,
            this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Scheduled " << result.count << " report definitions";
            emit self->statusMessage(tr("Scheduled %1 report definition(s).").arg(result.count));
            if (self->listWindow_)
                self->listWindow_->reload();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Schedule failed: " << result.message;
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void ReportDefinitionController::onUnscheduleRequested(
    const std::vector<boost::uuids::uuid>& ids) {

    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorMessage(tr("Cannot unschedule: not connected to server."));
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Unscheduling " << ids.size() << " report definitions";

    QPointer<ReportDefinitionController> self = this;
    const std::string performed_by = username_.toStdString();

    struct UnscheduleResult { bool success; std::string message; int count; };

    auto task = [self, ids, performed_by]() -> UnscheduleResult {
        if (!self || !self->clientManager_)
            return {false, "Controller destroyed", 0};

        reporting::messaging::unschedule_report_definitions_request request;
        for (const auto& id : ids) {
            request.ids.push_back(boost::uuids::to_string(id));
        }
        request.performed_by = performed_by;
        auto response_result = self->clientManager_->process_authenticated_request(std::move(request));
        if (!response_result)
            return {false, "Failed to communicate with server", 0};

        return {response_result->success, response_result->message, response_result->unscheduled_count};
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
            emit self->statusMessage(
                tr("Unscheduled %1 report definition(s).").arg(result.count));
            if (self->listWindow_)
                self->listWindow_->reload();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Unschedule failed: " << result.message;
            emit self->errorMessage(QString::fromStdString(result.message));
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

}
