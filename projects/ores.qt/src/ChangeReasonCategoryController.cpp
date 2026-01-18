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
#include "ores.qt/ChangeReasonCategoryController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ChangeReasonCategoryMdiWindow.hpp"
#include "ores.qt/ChangeReasonCategoryDetailDialog.hpp"
#include "ores.qt/ChangeReasonCategoryHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.dq/eventing/change_reason_category_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    // Event type name for change reason category changes
    constexpr std::string_view category_event_name =
        eventing::domain::event_traits<
            dq::eventing::change_reason_category_changed_event>::name;
}

ChangeReasonCategoryController::ChangeReasonCategoryController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QList<DetachableMdiSubWindow*>& allDetachableWindows,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr),
      allDetachableWindows_(allDetachableWindows) {

    BOOST_LOG_SEV(lg(), debug) << "ChangeReasonCategoryController created";

    // Connect to notification signal from ClientManager
    if (clientManager_) {
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &ChangeReasonCategoryController::onNotificationReceived);

        // Subscribe to events when connected
        connect(clientManager_, &ClientManager::connected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Subscribing to category change events";
            clientManager_->subscribeToEvent(std::string{category_event_name});
        });

        // Re-subscribe after reconnection
        connect(clientManager_, &ClientManager::reconnected,
                this, [this]() {
            BOOST_LOG_SEV(lg(), info) << "Re-subscribing to category change events";
            clientManager_->subscribeToEvent(std::string{category_event_name});
        });

        // If already connected, subscribe now
        if (clientManager_->isConnected()) {
            BOOST_LOG_SEV(lg(), info) << "Already connected, subscribing to events";
            clientManager_->subscribeToEvent(std::string{category_event_name});
        }
    }
}

ChangeReasonCategoryController::~ChangeReasonCategoryController() {
    BOOST_LOG_SEV(lg(), debug) << "ChangeReasonCategoryController destroyed";

    // Unsubscribe from events
    if (clientManager_) {
        BOOST_LOG_SEV(lg(), debug) << "Unsubscribing from category change events";
        clientManager_->unsubscribeFromEvent(std::string{category_event_name});
    }
}

void ChangeReasonCategoryController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "change_reason_categories");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new ChangeReasonCategoryMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &ChangeReasonCategoryMdiWindow::statusChanged,
            this, &ChangeReasonCategoryController::statusMessage);
    connect(listWindow_, &ChangeReasonCategoryMdiWindow::errorOccurred,
            this, &ChangeReasonCategoryController::errorMessage);
    connect(listWindow_, &ChangeReasonCategoryMdiWindow::showCategoryDetails,
            this, &ChangeReasonCategoryController::onShowDetails);
    connect(listWindow_, &ChangeReasonCategoryMdiWindow::addNewRequested,
            this, &ChangeReasonCategoryController::onAddNewRequested);
    connect(listWindow_, &ChangeReasonCategoryMdiWindow::showCategoryHistory,
            this, &ChangeReasonCategoryController::onShowHistory);

    // Create MDI subwindow
    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Change Reason Categories");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    allDetachableWindows_.append(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [this, key]() {
        untrack_window(key);
        allDetachableWindows_.removeOne(listMdiSubWindow_);
        listWindow_ = nullptr;
        listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Category list window created";
}

void ChangeReasonCategoryController::closeAllWindows() {
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

void ChangeReasonCategoryController::onShowDetails(
    const dq::domain::change_reason_category& category) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << category.code;
    showDetailWindow(category);
}

void ChangeReasonCategoryController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new category requested";
    showAddWindow();
}

void ChangeReasonCategoryController::onShowHistory(const QString& code) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << code.toStdString();
    showHistoryWindow(code);
}

void ChangeReasonCategoryController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new category";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new ChangeReasonCategoryDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &ChangeReasonCategoryDetailDialog::statusMessage,
            this, &ChangeReasonCategoryController::statusMessage);
    connect(detailDialog, &ChangeReasonCategoryDetailDialog::errorMessage,
            this, &ChangeReasonCategoryController::errorMessage);
    connect(detailDialog, &ChangeReasonCategoryDetailDialog::categorySaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Category saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Category");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<ChangeReasonCategoryController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self)
            self->allDetachableWindows_.removeAll(detailWindow);
    });

    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ChangeReasonCategoryController::showDetailWindow(
    const dq::domain::change_reason_category& category) {

    const QString identifier = QString::fromStdString(category.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << category.code;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new ChangeReasonCategoryDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setCategory(category);

    connect(detailDialog, &ChangeReasonCategoryDetailDialog::statusMessage,
            this, &ChangeReasonCategoryController::statusMessage);
    connect(detailDialog, &ChangeReasonCategoryDetailDialog::errorMessage,
            this, &ChangeReasonCategoryController::errorMessage);
    connect(detailDialog, &ChangeReasonCategoryDetailDialog::categorySaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Category saved: " << code.toStdString();
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    });
    connect(detailDialog, &ChangeReasonCategoryDetailDialog::categoryDeleted,
            this, [this, key](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Category deleted: " << code.toStdString();
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Category: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_tag_20_regular.svg", iconColor));

    // Track window
    track_window(key, detailWindow);
    allDetachableWindows_.append(detailWindow);

    QPointer<ChangeReasonCategoryController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow, key]() {
        if (self) {
            self->untrack_window(key);
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    show_managed_window(detailWindow, listMdiSubWindow_);
}

void ChangeReasonCategoryController::onNotificationReceived(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {

    if (eventType != QString::fromStdString(std::string{category_event_name})) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Received category change notification at "
                              << timestamp.toString(Qt::ISODate).toStdString()
                              << " with " << entityIds.size() << " category codes";

    // Mark the list window as stale
    if (listWindow_) {
        listWindow_->markAsStale();
        BOOST_LOG_SEV(lg(), debug) << "Marked category list as stale";
    }
}

void ChangeReasonCategoryController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for category: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << code.toStdString();
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new ChangeReasonCategoryHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &ChangeReasonCategoryHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyDialog, &ChangeReasonCategoryHistoryDialog::errorOccurred,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });
    connect(historyDialog, &ChangeReasonCategoryHistoryDialog::revertVersionRequested,
            this, &ChangeReasonCategoryController::onRevertVersion);
    connect(historyDialog, &ChangeReasonCategoryHistoryDialog::openVersionRequested,
            this, &ChangeReasonCategoryController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Category History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    // Track this history window
    track_window(windowKey, historyWindow);

    allDetachableWindows_.append(historyWindow);
    QPointer<ChangeReasonCategoryController> self = this;
    QPointer<DetachableMdiSubWindow> windowPtr = historyWindow;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowPtr, windowKey]() {
        if (self) {
            self->allDetachableWindows_.removeAll(windowPtr.data());
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void ChangeReasonCategoryController::onOpenVersion(
    const dq::domain::change_reason_category& category, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for category: " << category.code;

    const QString code = QString::fromStdString(category.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new ChangeReasonCategoryDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCategory(category);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &ChangeReasonCategoryDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &ChangeReasonCategoryDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Category: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    allDetachableWindows_.append(detailWindow);

    QPointer<ChangeReasonCategoryController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void ChangeReasonCategoryController::onRevertVersion(
    const dq::domain::change_reason_category& category) {
    BOOST_LOG_SEV(lg(), info) << "Reverting category to version: "
                              << category.version;

    // The history dialog already shows the confirmation and prepares the data.
    // We just need to save it as a new version.
    auto* detailDialog = new ChangeReasonCategoryDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCategory(category);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &ChangeReasonCategoryDetailDialog::statusMessage,
            this, &ChangeReasonCategoryController::statusMessage);
    connect(detailDialog, &ChangeReasonCategoryDetailDialog::errorMessage,
            this, &ChangeReasonCategoryController::errorMessage);
    connect(detailDialog, &ChangeReasonCategoryDetailDialog::categorySaved,
            this, [this](const QString& code) {
        BOOST_LOG_SEV(lg(), info) << "Category reverted: " << code.toStdString();
        emit statusMessage(QString("Category '%1' reverted successfully").arg(code));
        if (listWindow_) {
            listWindow_->markAsStale();
        }
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Category: %1")
        .arg(QString::fromStdString(category.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<ChangeReasonCategoryController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self) {
            self->allDetachableWindows_.removeAll(detailWindow);
        }
    });

    show_managed_window(detailWindow, listMdiSubWindow_);
}

}
