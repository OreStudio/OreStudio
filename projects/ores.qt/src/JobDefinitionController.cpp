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
#include "ores.qt/JobDefinitionController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/JobDefinitionMdiWindow.hpp"
#include "ores.qt/JobDefinitionDetailDialog.hpp"
#include "ores.qt/JobDefinitionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

JobDefinitionController::JobDefinitionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "JobDefinitionController created";
}

void JobDefinitionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "job_definitions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new JobDefinitionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &JobDefinitionMdiWindow::statusChanged,
            this, &JobDefinitionController::statusMessage);
    connect(listWindow_, &JobDefinitionMdiWindow::errorOccurred,
            this, &JobDefinitionController::errorMessage);
    connect(listWindow_, &JobDefinitionMdiWindow::showDefinitionDetails,
            this, &JobDefinitionController::onShowDetails);
    connect(listWindow_, &JobDefinitionMdiWindow::addNewRequested,
            this, &JobDefinitionController::onAddNewRequested);
    connect(listWindow_, &JobDefinitionMdiWindow::showDefinitionHistory,
            this, &JobDefinitionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Job Definitions");
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
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<JobDefinitionController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Job Definition list window created";
}

void JobDefinitionController::closeAllWindows() {
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

void JobDefinitionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void JobDefinitionController::onShowDetails(
    const scheduler::domain::job_definition& definition) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << definition.job_name;
    showDetailWindow(definition);
}

void JobDefinitionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new job definition requested";
    showAddWindow();
}

void JobDefinitionController::onShowHistory(
    const scheduler::domain::job_definition& definition) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << definition.job_name;
    showHistoryWindow(definition);
}

void JobDefinitionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new job definition";

    auto* detailDialog = new JobDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &JobDefinitionDetailDialog::statusMessage,
            this, &JobDefinitionController::statusMessage);
    connect(detailDialog, &JobDefinitionDetailDialog::errorMessage,
            this, &JobDefinitionController::errorMessage);
    connect(detailDialog, &JobDefinitionDetailDialog::definitionSaved,
            this, [self = QPointer<JobDefinitionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Job Definition saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Job Definition");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::CalendarClock, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void JobDefinitionController::showDetailWindow(
    const scheduler::domain::job_definition& definition) {

    const QString identifier = QString::fromStdString(definition.job_name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << definition.job_name;

    auto* detailDialog = new JobDefinitionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setDefinition(definition);

    connect(detailDialog, &JobDefinitionDetailDialog::statusMessage,
            this, &JobDefinitionController::statusMessage);
    connect(detailDialog, &JobDefinitionDetailDialog::errorMessage,
            this, &JobDefinitionController::errorMessage);
    connect(detailDialog, &JobDefinitionDetailDialog::definitionSaved,
            this, [self = QPointer<JobDefinitionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Job Definition saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &JobDefinitionDetailDialog::definitionDeleted,
            this, [self = QPointer<JobDefinitionController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Job Definition deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Job Definition: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::CalendarClock, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<JobDefinitionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void JobDefinitionController::showHistoryWindow(
    const scheduler::domain::job_definition& definition) {
    const QString code = QString::fromStdString(definition.job_name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for job definition: "
                              << definition.job_name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << definition.job_name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << definition.job_name;

    auto* historyDialog = new JobDefinitionHistoryDialog(
        definition.id, code, clientManager_, mainWindow_);
    // No revert/open-version signals — execution history is read-only

    connect(historyDialog, &JobDefinitionHistoryDialog::statusChanged,
            this, [self = QPointer<JobDefinitionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &JobDefinitionHistoryDialog::errorOccurred,
            this, [self = QPointer<JobDefinitionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Job Definition History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<JobDefinitionController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}


EntityListMdiWindow* JobDefinitionController::listWindow() const {
    return listWindow_;
}

}
