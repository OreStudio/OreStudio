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
#include "ores.qt/PortfolioController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PortfolioMdiWindow.hpp"
#include "ores.qt/PortfolioDetailDialog.hpp"
#include "ores.qt/PortfolioHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

PortfolioController::PortfolioController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ImageCache* imageCache,
    ChangeReasonCache* changeReasonCache,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      imageCache_(imageCache),
      changeReasonCache_(changeReasonCache),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PortfolioController created";
}

void PortfolioController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "portfolios");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PortfolioMdiWindow(clientManager_, imageCache_, username_);

    // Connect signals
    connect(listWindow_, &PortfolioMdiWindow::statusChanged,
            this, &PortfolioController::statusMessage);
    connect(listWindow_, &PortfolioMdiWindow::errorOccurred,
            this, &PortfolioController::errorMessage);
    connect(listWindow_, &PortfolioMdiWindow::showPortfolioDetails,
            this, &PortfolioController::onShowDetails);
    connect(listWindow_, &PortfolioMdiWindow::addNewRequested,
            this, &PortfolioController::onAddNewRequested);
    connect(listWindow_, &PortfolioMdiWindow::showPortfolioHistory,
            this, &PortfolioController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Portfolios");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Briefcase, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<PortfolioController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Portfolio list window created";
}

void PortfolioController::closeAllWindows() {
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

void PortfolioController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PortfolioController::openAdd()                                             { showAddWindow(); }
void PortfolioController::openAddWithParent(boost::uuids::uuid parent_id)       { showAddWindow(parent_id); }
void PortfolioController::openEdit(const refdata::domain::portfolio& p)         { showDetailWindow(p); }
void PortfolioController::openHistory(const refdata::domain::portfolio& p)      { showHistoryWindow(p); }

void PortfolioController::onShowDetails(
    const refdata::domain::portfolio& portfolio) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << portfolio.name;
    showDetailWindow(portfolio);
}

void PortfolioController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new portfolio requested";
    showAddWindow();
}

void PortfolioController::onShowHistory(
    const refdata::domain::portfolio& portfolio) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << portfolio.name;
    showHistoryWindow(portfolio);
}

void PortfolioController::showAddWindow(boost::uuids::uuid parentPortfolioId) {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new portfolio";

    auto* detailDialog = new PortfolioDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setChangeReasonCache(changeReasonCache_);

    if (!parentPortfolioId.is_nil()) {
        refdata::domain::portfolio prefilled;
        prefilled.parent_portfolio_id = parentPortfolioId;
        detailDialog->setPortfolio(prefilled);
    }

    detailDialog->setCreateMode(true);

    connect(detailDialog, &PortfolioDetailDialog::statusMessage,
            this, &PortfolioController::statusMessage);
    connect(detailDialog, &PortfolioDetailDialog::errorMessage,
            this, &PortfolioController::errorMessage);
    connect(detailDialog, &PortfolioDetailDialog::portfolioSaved,
            this, [self = QPointer<PortfolioController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Portfolio saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Portfolio");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Briefcase, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PortfolioController::showDetailWindow(
    const refdata::domain::portfolio& portfolio) {

    const QString identifier = QString::fromStdString(portfolio.name);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << portfolio.name;

    auto* detailDialog = new PortfolioDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setCreateMode(false);
    detailDialog->setPortfolio(portfolio);

    connect(detailDialog, &PortfolioDetailDialog::statusMessage,
            this, &PortfolioController::statusMessage);
    connect(detailDialog, &PortfolioDetailDialog::errorMessage,
            this, &PortfolioController::errorMessage);
    connect(detailDialog, &PortfolioDetailDialog::portfolioSaved,
            this, [self = QPointer<PortfolioController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Portfolio saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &PortfolioDetailDialog::portfolioDeleted,
            this, [self = QPointer<PortfolioController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Portfolio deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Portfolio: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Briefcase, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PortfolioController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PortfolioController::showHistoryWindow(
    const refdata::domain::portfolio& portfolio) {
    const QString code = QString::fromStdString(portfolio.name);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for portfolio: "
                              << portfolio.name;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << portfolio.name;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << portfolio.name;

    auto* historyDialog = new PortfolioHistoryDialog(
        portfolio.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &PortfolioHistoryDialog::statusChanged,
            this, [self = QPointer<PortfolioController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &PortfolioHistoryDialog::errorOccurred,
            this, [self = QPointer<PortfolioController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &PortfolioHistoryDialog::revertVersionRequested,
            this, &PortfolioController::onRevertVersion);
    connect(historyDialog, &PortfolioHistoryDialog::openVersionRequested,
            this, &PortfolioController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Portfolio History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<PortfolioController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PortfolioController::onOpenVersion(
    const refdata::domain::portfolio& portfolio, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for portfolio: " << portfolio.name;

    const QString code = QString::fromStdString(portfolio.name);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PortfolioDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setPortfolio(portfolio);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &PortfolioDetailDialog::statusMessage,
            this, [self = QPointer<PortfolioController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &PortfolioDetailDialog::errorMessage,
            this, [self = QPointer<PortfolioController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Portfolio: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PortfolioController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PortfolioController::onRevertVersion(
    const refdata::domain::portfolio& portfolio) {
    BOOST_LOG_SEV(lg(), info) << "Reverting portfolio to version: "
                              << portfolio.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PortfolioDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setImageCache(imageCache_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setChangeReasonCache(changeReasonCache_);
    detailDialog->setPortfolio(portfolio);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &PortfolioDetailDialog::statusMessage,
            this, &PortfolioController::statusMessage);
    connect(detailDialog, &PortfolioDetailDialog::errorMessage,
            this, &PortfolioController::errorMessage);
    connect(detailDialog, &PortfolioDetailDialog::portfolioSaved,
            this, [self = QPointer<PortfolioController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Portfolio reverted: " << code.toStdString();
        emit self->statusMessage(QString("Portfolio '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Portfolio: %1")
        .arg(QString::fromStdString(portfolio.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PortfolioController::listWindow() const {
    return listWindow_;
}

}
