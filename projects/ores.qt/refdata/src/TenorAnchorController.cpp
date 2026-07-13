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
#include "ores.qt/TenorAnchorController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TenorAnchorDetailDialog.hpp"
#include "ores.qt/TenorAnchorHistoryDialog.hpp"
#include "ores.qt/TenorAnchorMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/tenor_anchor_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view anchor_event_name =
    eventing::domain::event_traits<refdata::eventing::tenor_anchor_changed_event>::name;
}

TenorAnchorController::TenorAnchorController(QMainWindow* mainWindow,
                                             QMdiArea* mdiArea,
                                             ClientManager* clientManager,
                                             ChangeReasonCache* changeReasonCache,
                                             const QString& username,
                                             QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, anchor_event_name, parent)
    , changeReasonCache_(changeReasonCache)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "TenorAnchorController created";
}

void TenorAnchorController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "tenor_anchors");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new TenorAnchorMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &TenorAnchorMdiWindow::statusChanged,
            this,
            &TenorAnchorController::statusMessage);
    connect(listWindow_,
            &TenorAnchorMdiWindow::errorOccurred,
            this,
            &TenorAnchorController::errorMessage);
    connect(listWindow_,
            &TenorAnchorMdiWindow::showAnchorDetails,
            this,
            &TenorAnchorController::onShowDetails);
    connect(listWindow_,
            &TenorAnchorMdiWindow::addNewRequested,
            this,
            &TenorAnchorController::onAddNewRequested);
    connect(listWindow_,
            &TenorAnchorMdiWindow::showAnchorHistory,
            this,
            &TenorAnchorController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Tenor Anchors");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Anchor, IconUtils::DefaultIconColor));
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
            [self = QPointer<TenorAnchorController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Tenor Anchor list window created";
}

void TenorAnchorController::closeAllWindows() {
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

void TenorAnchorController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TenorAnchorController::onShowDetails(const refdata::domain::tenor_anchor& anchor) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << anchor.code;
    showDetailWindow(anchor);
}

void TenorAnchorController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new tenor anchor requested";
    showAddWindow();
}


void TenorAnchorController::onShowHistory(const refdata::domain::tenor_anchor& anchor) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << anchor.code;
    showHistoryWindow(QString::fromStdString(anchor.code));
}

void TenorAnchorController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new tenor anchor";

    auto* detailDialog = new TenorAnchorDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &TenorAnchorDetailDialog::statusMessage,
            this,
            &TenorAnchorController::statusMessage);
    connect(detailDialog,
            &TenorAnchorDetailDialog::errorMessage,
            this,
            &TenorAnchorController::errorMessage);
    connect(detailDialog,
            &TenorAnchorDetailDialog::anchorSaved,
            this,
            [self = QPointer<TenorAnchorController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Anchor saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Tenor Anchor");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Anchor, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenorAnchorController::showDetailWindow(const refdata::domain::tenor_anchor& anchor) {

    const QString identifier = QString::fromStdString(anchor.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << anchor.code;

    auto* detailDialog = new TenorAnchorDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setAnchor(anchor);

    connect(detailDialog,
            &TenorAnchorDetailDialog::statusMessage,
            this,
            &TenorAnchorController::statusMessage);
    connect(detailDialog,
            &TenorAnchorDetailDialog::errorMessage,
            this,
            &TenorAnchorController::errorMessage);
    connect(detailDialog,
            &TenorAnchorDetailDialog::anchorSaved,
            this,
            [self = QPointer<TenorAnchorController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Anchor saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &TenorAnchorDetailDialog::anchorDeleted,
            this,
            [self = QPointer<TenorAnchorController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Anchor deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenor Anchor: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Anchor, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<TenorAnchorController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenorAnchorController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for tenor anchor: " << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new TenorAnchorHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &TenorAnchorHistoryDialog::statusChanged,
            this,
            [self = QPointer<TenorAnchorController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &TenorAnchorHistoryDialog::errorOccurred,
            this,
            [self = QPointer<TenorAnchorController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &TenorAnchorHistoryDialog::revertVersionRequested,
            this,
            &TenorAnchorController::onRevertVersion);
    connect(historyDialog,
            &TenorAnchorHistoryDialog::openVersionRequested,
            this,
            &TenorAnchorController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Tenor Anchor History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<TenorAnchorController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void TenorAnchorController::onOpenVersion(const refdata::domain::tenor_anchor& anchor,
                                          int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for tenor anchor: " << anchor.code;

    const QString code = QString::fromStdString(anchor.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new TenorAnchorDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setAnchor(anchor);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &TenorAnchorDetailDialog::statusMessage,
            this,
            [self = QPointer<TenorAnchorController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &TenorAnchorDetailDialog::errorMessage,
            this,
            [self = QPointer<TenorAnchorController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Tenor Anchor: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TenorAnchorController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void TenorAnchorController::onRevertVersion(const refdata::domain::tenor_anchor& anchor) {
    BOOST_LOG_SEV(lg(), info) << "Reverting tenor anchor to version: " << anchor.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new TenorAnchorDetailDialog(mainWindow_);
    if (changeReasonCache_)
        detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_anchor = anchor;
    reverted_anchor.version = 0;
    detailDialog->setAnchor(reverted_anchor);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &TenorAnchorDetailDialog::statusMessage,
            this,
            &TenorAnchorController::statusMessage);
    connect(detailDialog,
            &TenorAnchorDetailDialog::errorMessage,
            this,
            &TenorAnchorController::errorMessage);
    connect(detailDialog,
            &TenorAnchorDetailDialog::anchorSaved,
            this,
            [self = QPointer<TenorAnchorController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Anchor reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Tenor Anchor '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Tenor Anchor: %1").arg(QString::fromStdString(anchor.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* TenorAnchorController::listWindow() const {
    return listWindow_;
}

void TenorAnchorController::notifyOpenDialogs(const QStringList& entityIds) {
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        auto* window = it.value();
        if (!window)
            continue;

        if (it.key().startsWith("details.")) {
            if (auto* dialog = qobject_cast<DetailDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        } else if (it.key().startsWith("history.")) {
            if (auto* dialog = qobject_cast<HistoryDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        }
    }
}

}
