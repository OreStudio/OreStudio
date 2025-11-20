/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CurrencyController.hpp"

#include <QPointer>
#include "ores.qt/CurrencyMdiWindow.hpp"
#include "ores.qt/CurrencyDetailDialog.hpp"
#include "ores.qt/CurrencyHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::utility::log;

CurrencyController::CurrencyController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    std::shared_ptr<comms::net::client> client,
    const QString& username,
    QList<DetachableMdiSubWindow*>& allDetachableWindows,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, client, username, parent),
      allDetachableWindows_(allDetachableWindows),
      currencyListWindow_(nullptr) {
    BOOST_LOG_SEV(lg(), debug) << "Currency controller created";
}

CurrencyController::~CurrencyController() {
    BOOST_LOG_SEV(lg(), debug) << "Currency controller destroyed";
}

void CurrencyController::showListWindow() {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), warn) << "Currencies action triggered but not connected";
        MessageBoxHelper::warning(mainWindow_, "Not Connected",
            "Please login first to view currencies.");
        return;
    }

    // Reuse existing window if it exists
    if (currencyListWindow_) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing currencies window";

        // Bring window to front
        if (currencyListWindow_->isDetached()) {
            currencyListWindow_->setVisible(true);
            currencyListWindow_->show();
            currencyListWindow_->raise();
            currencyListWindow_->activateWindow();
        } else {
            currencyListWindow_->setVisible(true);
            mdiArea_->setActiveSubWindow(currencyListWindow_);
            currencyListWindow_->show();
            currencyListWindow_->raise();
        }
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new currencies MDI window";
    const QColor iconColor(220, 220, 220);
    auto* currencyWidget = new CurrencyMdiWindow(client_, mainWindow_);

    // Connect status signals
    connect(currencyWidget, &CurrencyMdiWindow::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(currencyWidget, &CurrencyMdiWindow::errorOccurred,
            this, [this](const QString& err_msg) {
        emit errorMessage("Error loading currencies: " + err_msg);
    });

    // Connect currency operations (add, edit, history)
    connect(currencyWidget, &CurrencyMdiWindow::addNewRequested,
            this, &CurrencyController::onAddNewRequested);
    connect(currencyWidget, &CurrencyMdiWindow::showCurrencyDetails,
            this, &CurrencyController::onShowCurrencyDetails);
    connect(currencyWidget, &CurrencyMdiWindow::showCurrencyHistory,
            this, &CurrencyController::onShowCurrencyHistory);

    currencyListWindow_ = new DetachableMdiSubWindow();
    currencyListWindow_->setWidget(currencyWidget);
    currencyListWindow_->setWindowTitle("Currencies");
    currencyListWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

    // Track window for detach/reattach operations
    allDetachableWindows_.append(currencyListWindow_);
    QPointer<CurrencyController> self = this;
    QPointer<DetachableMdiSubWindow> windowBeingDestroyed = currencyListWindow_;
    connect(currencyListWindow_, &QObject::destroyed, this,
        [self, windowBeingDestroyed]() {
        if (!self)
            return;

        if (!windowBeingDestroyed.isNull()) {
            self->allDetachableWindows_.removeAll(windowBeingDestroyed.data());
        }

        if (self->currencyListWindow_ == windowBeingDestroyed)
            self->currencyListWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(currencyListWindow_);
    currencyListWindow_->adjustSize();
    currencyListWindow_->show();
}

void CurrencyController::closeAllWindows() {
    if (currencyListWindow_) {
        currencyListWindow_->close();
    }
}

void CurrencyController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new currency requested";
    const QColor iconColor(220, 220, 220);
    risk::domain::currency new_currency;

    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (client_) {
        detailDialog->setClient(client_);
        detailDialog->setUsername(username_.toStdString());
    }

    connect(detailDialog, &CurrencyDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CurrencyDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });

    detailDialog->setCurrency(new_currency);

    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Currency");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<CurrencyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self)
            self->allDetachableWindows_.removeAll(detailWindow);
    });

    mdiArea_->addSubWindow(detailWindow);
    detailWindow->adjustSize();

    // If the parent currency list window is detached, detach this window too
    // and position it near the parent
    if (currencyListWindow_ && currencyListWindow_->isDetached()) {
        detailWindow->show();  // Show first so geometry is valid
        detailWindow->detach();

        // Position near parent with offset
        QPoint parentPos = currencyListWindow_->pos();
        detailWindow->move(parentPos.x() + 30, parentPos.y() + 30);
    } else
        detailWindow->show();
}

void CurrencyController::onShowCurrencyDetails(
    const risk::domain::currency& currency) {
    BOOST_LOG_SEV(lg(), info) << "Showing currency details for: "
                             << currency.iso_code;
    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new CurrencyDetailDialog(mainWindow_);
    if (client_) {
        detailDialog->setClient(client_);
        detailDialog->setUsername(username_.toStdString());
    }

    connect(detailDialog, &CurrencyDetailDialog::statusMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(detailDialog, &CurrencyDetailDialog::errorMessage,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });

    detailDialog->setCurrency(currency);

    const QString iso_code = QString::fromStdString(currency.iso_code);
    auto* detailWindow = new DetachableMdiSubWindow();
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Currency Details: %1").arg(iso_code));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_currency_dollar_euro_20_filled.svg", iconColor));

    allDetachableWindows_.append(detailWindow);
    QPointer<CurrencyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, detailWindow]() {
        if (self)
            self->allDetachableWindows_.removeAll(detailWindow);
    });

    mdiArea_->addSubWindow(detailWindow);
    detailWindow->adjustSize();

    // If the parent currency list window is detached, detach this window too
    // and position it near the parent
    if (currencyListWindow_ && currencyListWindow_->isDetached()) {
        detailWindow->show();  // Show first so geometry is valid
        detailWindow->detach();

        // Position near parent with offset
        QPoint parentPos = currencyListWindow_->pos();
        detailWindow->move(parentPos.x() + 30, parentPos.y() + 30);
    } else {
        detailWindow->show();
    }
}

void CurrencyController::onShowCurrencyHistory(const QString& isoCode) {
    BOOST_LOG_SEV(lg(), info) << "Showing currency history for: "
                             << isoCode.toStdString();

    if (!client_ || !client_->is_connected()) {
        MessageBoxHelper::warning(mainWindow_, "Not Connected",
            "Please ensure you are still connected to view currency history.");
        return;
    }

    // Reuse existing history window if it exists
    if (currencyHistoryWindows_.contains(isoCode)) {
        auto existingWindow = currencyHistoryWindows_[isoCode];
        if (existingWindow) {
            BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                      << isoCode.toStdString();

            // Bring window to front
            if (existingWindow->isDetached()) {
                existingWindow->setVisible(true);
                existingWindow->show();
                existingWindow->raise();
                existingWindow->activateWindow();
            } else {
                existingWindow->setVisible(true);
                mdiArea_->setActiveSubWindow(existingWindow);
                existingWindow->show();
                existingWindow->raise();
            }
            return;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << isoCode.toStdString();
    const QColor iconColor(220, 220, 220);

    auto* historyWidget = new CurrencyHistoryDialog(isoCode, client_,
                                                     mainWindow_);

    connect(historyWidget, &CurrencyHistoryDialog::statusChanged,
            this, [this](const QString& message) {
        emit statusMessage(message);
    });
    connect(historyWidget, &CurrencyHistoryDialog::errorOccurred,
            this, [this](const QString& error_message) {
        emit statusMessage("Error loading history: " + error_message);
    });

    historyWidget->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow();
    historyWindow->setWidget(historyWidget);
    historyWindow->setWindowTitle(QString("History: %1").arg(isoCode));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    // Track this history window
    currencyHistoryWindows_[isoCode] = historyWindow;

    allDetachableWindows_.append(historyWindow);
    QPointer<CurrencyController> self = this;
    QPointer<DetachableMdiSubWindow> windowPtr = historyWindow;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowPtr, isoCode]() {
        if (self) {
            self->allDetachableWindows_.removeAll(windowPtr.data());
            self->currencyHistoryWindows_.remove(isoCode);
        }
    });

    mdiArea_->addSubWindow(historyWindow);
    historyWindow->adjustSize();

    // If the parent currency list window is detached, detach this window too
    // and position it near the parent
    if (currencyListWindow_ && currencyListWindow_->isDetached()) {
        historyWindow->show();  // Show first so geometry is valid
        historyWindow->detach();

        // Position near parent with offset
        QPoint parentPos = currencyListWindow_->pos();
        historyWindow->move(parentPos.x() + 30, parentPos.y() + 30);
    } else {
        historyWindow->show();
    }
}

}
