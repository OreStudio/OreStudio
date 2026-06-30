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
#include "ores.qt/GmmComponentController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/GmmComponentDetailDialog.hpp"
#include "ores.qt/GmmComponentHistoryDialog.hpp"
#include "ores.qt/GmmComponentMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.synthetic.api/eventing/gmm_component_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view gmm_component_event_name =
    eventing::domain::event_traits<synthetic::eventing::gmm_component_changed_event>::name;
}

GmmComponentController::GmmComponentController(QMainWindow* mainWindow,
                                               QMdiArea* mdiArea,
                                               ClientManager* clientManager,
                                               ChangeReasonCache* changeReasonCache,
                                               const QString& username,
                                               QObject* parent)
    : EntityController(
          mainWindow, mdiArea, clientManager, username, gmm_component_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "GmmComponentController created";
}

void GmmComponentController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "gmm_components");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new GmmComponentMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &GmmComponentMdiWindow::statusChanged,
            this,
            &GmmComponentController::statusMessage);
    connect(listWindow_,
            &GmmComponentMdiWindow::errorOccurred,
            this,
            &GmmComponentController::errorMessage);
    connect(listWindow_,
            &GmmComponentMdiWindow::showComponentDetails,
            this,
            &GmmComponentController::onShowDetails);
    connect(listWindow_,
            &GmmComponentMdiWindow::addNewRequested,
            this,
            &GmmComponentController::onAddNewRequested);
    connect(listWindow_,
            &GmmComponentMdiWindow::showComponentHistory,
            this,
            &GmmComponentController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("GMM Components");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<GmmComponentController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "GMM Component list window created";
}

void GmmComponentController::closeAllWindows() {
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

void GmmComponentController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void GmmComponentController::onShowDetails(const synthetic::domain::gmm_component& gmm_component) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << boost::uuids::to_string(gmm_component.id);
    showDetailWindow(gmm_component);
}

void GmmComponentController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new GMM component requested";
    showAddWindow();
}

void GmmComponentController::onShowHistory(const synthetic::domain::gmm_component& gmm_component) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << boost::uuids::to_string(gmm_component.id);
    showHistoryWindow(gmm_component);
}

void GmmComponentController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new GMM component";

    auto* detailDialog = new GmmComponentDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &GmmComponentDetailDialog::statusMessage,
            this,
            &GmmComponentController::statusMessage);
    connect(detailDialog,
            &GmmComponentDetailDialog::errorMessage,
            this,
            &GmmComponentController::errorMessage);
    connect(detailDialog,
            &GmmComponentDetailDialog::gmm_componentSaved,
            this,
            [self = QPointer<GmmComponentController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "GMM Component saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New GMM Component");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void GmmComponentController::showDetailWindow(
    const synthetic::domain::gmm_component& gmm_component) {

    const QString identifier = QString::fromStdString(boost::uuids::to_string(gmm_component.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << boost::uuids::to_string(gmm_component.id);

    auto* detailDialog = new GmmComponentDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setComponent(gmm_component);

    connect(detailDialog,
            &GmmComponentDetailDialog::statusMessage,
            this,
            &GmmComponentController::statusMessage);
    connect(detailDialog,
            &GmmComponentDetailDialog::errorMessage,
            this,
            &GmmComponentController::errorMessage);
    connect(detailDialog,
            &GmmComponentDetailDialog::gmm_componentSaved,
            this,
            [self = QPointer<GmmComponentController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "GMM Component saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &GmmComponentDetailDialog::gmm_componentDeleted,
            this,
            [self = QPointer<GmmComponentController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "GMM Component deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("GMM Component: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Chart, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<GmmComponentController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void GmmComponentController::showHistoryWindow(
    const synthetic::domain::gmm_component& gmm_component) {
    const QString code = QString::fromStdString(boost::uuids::to_string(gmm_component.id));
    BOOST_LOG_SEV(lg(), info) << "Opening history window for GMM component: "
                              << boost::uuids::to_string(gmm_component.id);

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << boost::uuids::to_string(gmm_component.id);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << boost::uuids::to_string(gmm_component.id);

    auto* historyDialog =
        new GmmComponentHistoryDialog(gmm_component.id, code, clientManager_, mainWindow_);

    connect(historyDialog,
            &GmmComponentHistoryDialog::statusChanged,
            this,
            [self = QPointer<GmmComponentController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &GmmComponentHistoryDialog::errorOccurred,
            this,
            [self = QPointer<GmmComponentController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &GmmComponentHistoryDialog::revertVersionRequested,
            this,
            &GmmComponentController::onRevertVersion);
    connect(historyDialog,
            &GmmComponentHistoryDialog::openVersionRequested,
            this,
            &GmmComponentController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("GMM Component History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<GmmComponentController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void GmmComponentController::onOpenVersion(const synthetic::domain::gmm_component& gmm_component,
                                           int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for GMM component: "
                              << boost::uuids::to_string(gmm_component.id);

    const QString code = QString::fromStdString(boost::uuids::to_string(gmm_component.id));
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new GmmComponentDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setComponent(gmm_component);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &GmmComponentDetailDialog::statusMessage,
            this,
            [self = QPointer<GmmComponentController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &GmmComponentDetailDialog::errorMessage,
            this,
            [self = QPointer<GmmComponentController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("GMM Component: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<GmmComponentController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void GmmComponentController::onRevertVersion(
    const synthetic::domain::gmm_component& gmm_component) {
    BOOST_LOG_SEV(lg(), info) << "Reverting GMM component to version: " << gmm_component.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new GmmComponentDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_gmm_component = gmm_component;
    reverted_gmm_component.version = 0;
    detailDialog->setComponent(reverted_gmm_component);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &GmmComponentDetailDialog::statusMessage,
            this,
            &GmmComponentController::statusMessage);
    connect(detailDialog,
            &GmmComponentDetailDialog::errorMessage,
            this,
            &GmmComponentController::errorMessage);
    connect(detailDialog,
            &GmmComponentDetailDialog::gmm_componentSaved,
            this,
            [self = QPointer<GmmComponentController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "GMM Component reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("GMM Component '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert GMM Component: %1")
            .arg(QString::fromStdString(boost::uuids::to_string(gmm_component.id))));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* GmmComponentController::listWindow() const {
    return listWindow_;
}

}
