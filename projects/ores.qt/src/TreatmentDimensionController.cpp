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
#include "ores.qt/TreatmentDimensionController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TreatmentDimensionMdiWindow.hpp"
#include "ores.qt/TreatmentDimensionDetailDialog.hpp"
#include "ores.qt/TreatmentDimensionHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.dq/eventing/treatment_dimension_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view treatment_dimension_event_name =
        eventing::domain::event_traits<
            dq::eventing::treatment_dimension_changed_event>::name;
}

TreatmentDimensionController::TreatmentDimensionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          treatment_dimension_event_name, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "TreatmentDimensionController created";
}

EntityListMdiWindow* TreatmentDimensionController::listWindow() const {
    return listWindow_;
}

void TreatmentDimensionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "treatment_dimensions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new TreatmentDimensionMdiWindow(clientManager_, username_);

    connect(listWindow_, &TreatmentDimensionMdiWindow::statusChanged,
            this, &TreatmentDimensionController::statusMessage);
    connect(listWindow_, &TreatmentDimensionMdiWindow::errorOccurred,
            this, &TreatmentDimensionController::errorMessage);
    connect(listWindow_, &TreatmentDimensionMdiWindow::showDimensionDetails,
            this, &TreatmentDimensionController::onShowDetails);
    connect(listWindow_, &TreatmentDimensionMdiWindow::addNewRequested,
            this, &TreatmentDimensionController::onAddNewRequested);
    connect(listWindow_, &TreatmentDimensionMdiWindow::showDimensionHistory,
            this, &TreatmentDimensionController::onShowHistory);

    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Treatment Dimensions");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<TreatmentDimensionController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Treatment dimension list window created";
}

void TreatmentDimensionController::closeAllWindows() {
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

void TreatmentDimensionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TreatmentDimensionController::onShowDetails(
    const dq::domain::treatment_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << dimension.code;
    showDetailWindow(dimension);
}

void TreatmentDimensionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new treatment dimension requested";
    showAddWindow();
}

void TreatmentDimensionController::onShowHistory(const QString& code) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << code.toStdString();
    showHistoryWindow(code);
}

void TreatmentDimensionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new treatment dimension";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new TreatmentDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &TreatmentDimensionDetailDialog::statusMessage,
            this, &TreatmentDimensionController::statusMessage);
    connect(detailDialog, &TreatmentDimensionDetailDialog::errorMessage,
            this, &TreatmentDimensionController::errorMessage);
    connect(detailDialog, &TreatmentDimensionDetailDialog::dimensionSaved,
            this, [self = QPointer<TreatmentDimensionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Treatment dimension saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Treatment Dimension");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TreatmentDimensionController::showDetailWindow(
    const dq::domain::treatment_dimension& dimension) {

    const QString identifier = QString::fromStdString(dimension.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << dimension.code;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new TreatmentDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setDimension(dimension);

    connect(detailDialog, &TreatmentDimensionDetailDialog::statusMessage,
            this, &TreatmentDimensionController::statusMessage);
    connect(detailDialog, &TreatmentDimensionDetailDialog::errorMessage,
            this, &TreatmentDimensionController::errorMessage);
    connect(detailDialog, &TreatmentDimensionDetailDialog::dimensionSaved,
            this, [self = QPointer<TreatmentDimensionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Treatment dimension saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &TreatmentDimensionDetailDialog::dimensionDeleted,
            this, [self = QPointer<TreatmentDimensionController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Treatment dimension deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Treatment Dimension: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_database_20_regular.svg", iconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TreatmentDimensionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TreatmentDimensionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for treatment dimension: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << code.toStdString();
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new TreatmentDimensionHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog, &TreatmentDimensionHistoryDialog::statusChanged,
            this, [self = QPointer<TreatmentDimensionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &TreatmentDimensionHistoryDialog::errorOccurred,
            this, [self = QPointer<TreatmentDimensionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &TreatmentDimensionHistoryDialog::revertVersionRequested,
            this, &TreatmentDimensionController::onRevertVersion);
    connect(historyDialog, &TreatmentDimensionHistoryDialog::openVersionRequested,
            this, &TreatmentDimensionController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Treatment Dimension History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<TreatmentDimensionController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void TreatmentDimensionController::onOpenVersion(
    const dq::domain::treatment_dimension& dimension, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for treatment dimension: " << dimension.code;

    const QString code = QString::fromStdString(dimension.code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new TreatmentDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDimension(dimension);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &TreatmentDimensionDetailDialog::statusMessage,
            this, [self = QPointer<TreatmentDimensionController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &TreatmentDimensionDetailDialog::errorMessage,
            this, [self = QPointer<TreatmentDimensionController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Treatment Dimension: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TreatmentDimensionController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void TreatmentDimensionController::onRevertVersion(
    const dq::domain::treatment_dimension& dimension) {
    BOOST_LOG_SEV(lg(), info) << "Reverting treatment dimension to version: "
                              << dimension.version;

    auto* detailDialog = new TreatmentDimensionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setDimension(dimension);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &TreatmentDimensionDetailDialog::statusMessage,
            this, &TreatmentDimensionController::statusMessage);
    connect(detailDialog, &TreatmentDimensionDetailDialog::errorMessage,
            this, &TreatmentDimensionController::errorMessage);
    connect(detailDialog, &TreatmentDimensionDetailDialog::dimensionSaved,
            this, [self = QPointer<TreatmentDimensionController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Treatment dimension reverted: " << code.toStdString();
        emit self->statusMessage(QString("Treatment dimension '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Treatment Dimension: %1")
        .arg(QString::fromStdString(dimension.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

}
