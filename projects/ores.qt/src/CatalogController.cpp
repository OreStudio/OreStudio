/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CatalogController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/CatalogMdiWindow.hpp"
#include "ores.qt/CatalogDetailDialog.hpp"
#include "ores.qt/CatalogHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.dq/eventing/catalog_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view catalog_event_name =
        eventing::domain::event_traits<
            dq::eventing::catalog_changed_event>::name;
}

CatalogController::CatalogController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CatalogController created";

    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &CatalogController::onNotificationReceived);

        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to catalog change events";
            clientManager_->subscribeToEvent(std::string{catalog_event_name});
        });

        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to catalog change events";
            clientManager_->subscribeToEvent(std::string{catalog_event_name});
        });

        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to events";
            clientManager_->subscribeToEvent(std::string{catalog_event_name});
        }
    }
}

CatalogController::~CatalogController() {
    BOOST_LOG_SEV(lg(), debug) << "CatalogController destroyed";

    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from catalog change events";
        clientManager_->unsubscribeFromEvent(std::string{catalog_event_name});
    }
}

void CatalogController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "catalogs");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new CatalogMdiWindow(clientManager_, username_);

    connect(listWindow_, &CatalogMdiWindow::statusChanged,
            this, &CatalogController::statusMessage);
    connect(listWindow_, &CatalogMdiWindow::errorOccurred,
            this, &CatalogController::errorMessage);
    connect(listWindow_, &CatalogMdiWindow::showCatalogDetails,
            this, &CatalogController::onShowDetails);
    connect(listWindow_, &CatalogMdiWindow::addNewRequested,
            this, &CatalogController::onAddNewRequested);
    connect(listWindow_, &CatalogMdiWindow::showCatalogHistory,
            this, &CatalogController::onShowHistory);

    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Catalogs");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_library_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed, this, [this, key]() {
        untrack_window(key);
        listWindow_ = nullptr;
        listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Catalog list window created";
}

void CatalogController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

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

void CatalogController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void CatalogController::onShowDetails(const dq::domain::catalog& catalog) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << catalog.name;
    showDetailWindow(catalog);
}

void CatalogController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new catalog requested";
    showAddWindow();
}

void CatalogController::onShowHistory(const QString& name) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << name.toStdString();
    showHistoryWindow(name);
}

void CatalogController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new catalog";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CatalogDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &CatalogDetailDialog::statusMessage,
            this, &CatalogController::statusMessage);
    connect(detailDialog, &CatalogDetailDialog::errorMessage,
            this, &CatalogController::errorMessage);
    connect(detailDialog, &CatalogDetailDialog::catalogSaved,
            this, [this](const QString& name) {
        BOOST_LOG_SEV(lg(), info) << "Catalog saved: " << name.toStdString();
        handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Catalog");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_library_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CatalogController::showDetailWindow(const dq::domain::catalog& catalog) {
    const QString identifier = QString::fromStdString(catalog.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << catalog.name;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CatalogDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setCatalog(catalog);

    connect(detailDialog, &CatalogDetailDialog::statusMessage,
            this, &CatalogController::statusMessage);
    connect(detailDialog, &CatalogDetailDialog::errorMessage,
            this, &CatalogController::errorMessage);
    connect(detailDialog, &CatalogDetailDialog::catalogSaved,
            this, [this](const QString& name) {
        BOOST_LOG_SEV(lg(), info) << "Catalog saved: " << name.toStdString();
        handleEntitySaved();
    });
    connect(detailDialog, &CatalogDetailDialog::catalogDeleted,
            this, [this, key](const QString& name) {
        BOOST_LOG_SEV(lg(), info) << "Catalog deleted: " << name.toStdString();
        handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Catalog: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_library_20_regular.svg", iconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CatalogController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CatalogController::showHistoryWindow(const QString& name) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for catalog: "
                              << name.toStdString();

    const QString windowKey = build_window_key("history", name);

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << name.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << name.toStdString();
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new CatalogHistoryDialog(
        name, clientManager_, mainWindow_);

    connect(historyDialog, &CatalogHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &CatalogHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &CatalogHistoryDialog::revertVersionRequested,
            this, &CatalogController::onRevertVersion);
    connect(historyDialog, &CatalogHistoryDialog::openVersionRequested,
            this, &CatalogController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Catalog History: %1").arg(name));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<CatalogController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void CatalogController::onOpenVersion(
    const dq::domain::catalog& catalog, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for catalog: " << catalog.name;

    const QString name = QString::fromStdString(catalog.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(name).arg(versionNumber));

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CatalogDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCatalog(catalog);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &CatalogDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CatalogDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Catalog: %1 (Version %2)")
        .arg(name).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<CatalogController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void CatalogController::onRevertVersion(const dq::domain::catalog& catalog) {
    BOOST_LOG_SEV(lg(), info) << "Reverting catalog to version: "
                              << catalog.version;

    auto* detailDialog = new CatalogDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCatalog(catalog);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &CatalogDetailDialog::statusMessage,
            this, &CatalogController::statusMessage);
    connect(detailDialog, &CatalogDetailDialog::errorMessage,
            this, &CatalogController::errorMessage);
    connect(detailDialog, &CatalogDetailDialog::catalogSaved,
            this, [this](const QString& name) {
        BOOST_LOG_SEV(lg(), info) << "Catalog reverted: " << name.toStdString();
        emit statusMessage(
            QString("Catalog '%1' reverted successfully").arg(name));
        handleEntitySaved();
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Catalog: %1")
        .arg(QString::fromStdString(catalog.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg",
        iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void CatalogController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {

    if (eventType != QString::fromStdString(std::string{catalog_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received catalog change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " IDs";

    if (listWindow_) {
        listWindow_->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked catalog list as stale";
    }
}

}
