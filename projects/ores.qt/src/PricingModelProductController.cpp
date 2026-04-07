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
#include "ores.qt/PricingModelProductController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/PricingModelProductMdiWindow.hpp"
#include "ores.qt/PricingModelProductDetailDialog.hpp"
#include "ores.qt/PricingModelProductHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

PricingModelProductController::PricingModelProductController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "PricingModelProductController created";
}

void PricingModelProductController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "pricing_model_products");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new PricingModelProductMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_, &PricingModelProductMdiWindow::statusChanged,
            this, &PricingModelProductController::statusMessage);
    connect(listWindow_, &PricingModelProductMdiWindow::errorOccurred,
            this, &PricingModelProductController::errorMessage);
    connect(listWindow_, &PricingModelProductMdiWindow::showProductDetails,
            this, &PricingModelProductController::onShowDetails);
    connect(listWindow_, &PricingModelProductMdiWindow::addNewRequested,
            this, &PricingModelProductController::onAddNewRequested);
    connect(listWindow_, &PricingModelProductMdiWindow::showProductHistory,
            this, &PricingModelProductController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Pricing Model Products");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Table, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<PricingModelProductController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Pricing Model Product list window created";
}

void PricingModelProductController::closeAllWindows() {
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

void PricingModelProductController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void PricingModelProductController::onShowDetails(
    const analytics::domain::pricing_model_product& product) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << product.pricing_engine_type_code;
    showDetailWindow(product);
}

void PricingModelProductController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new pricing model product requested";
    showAddWindow();
}

void PricingModelProductController::onShowHistory(
    const analytics::domain::pricing_model_product& product) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << product.pricing_engine_type_code;
    showHistoryWindow(product);
}

void PricingModelProductController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new pricing model product";

    auto* detailDialog = new PricingModelProductDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &PricingModelProductDetailDialog::statusMessage,
            this, &PricingModelProductController::statusMessage);
    connect(detailDialog, &PricingModelProductDetailDialog::errorMessage,
            this, &PricingModelProductController::errorMessage);
    connect(detailDialog, &PricingModelProductDetailDialog::productSaved,
            this, [self = QPointer<PricingModelProductController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Product saved: " << code.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Pricing Model Product");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Table, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PricingModelProductController::showDetailWindow(
    const analytics::domain::pricing_model_product& product) {

    const QString identifier = QString::fromStdString(product.pricing_engine_type_code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << product.pricing_engine_type_code;

    auto* detailDialog = new PricingModelProductDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setProduct(product);

    connect(detailDialog, &PricingModelProductDetailDialog::statusMessage,
            this, &PricingModelProductController::statusMessage);
    connect(detailDialog, &PricingModelProductDetailDialog::errorMessage,
            this, &PricingModelProductController::errorMessage);
    connect(detailDialog, &PricingModelProductDetailDialog::productSaved,
            this, [self = QPointer<PricingModelProductController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Product saved: " << code.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &PricingModelProductDetailDialog::productDeleted,
            this, [self = QPointer<PricingModelProductController>(this), key](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Product deleted: " << code.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Pricing Model Product: %1").arg(identifier));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Table, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PricingModelProductController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void PricingModelProductController::showHistoryWindow(
    const analytics::domain::pricing_model_product& product) {
    const QString code = QString::fromStdString(product.pricing_engine_type_code);
    BOOST_LOG_SEV(lg(), info) << "Opening history window for pricing model product: "
                              << product.pricing_engine_type_code;

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << product.pricing_engine_type_code;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << product.pricing_engine_type_code;

    auto* historyDialog = new PricingModelProductHistoryDialog(
        product.id, code, clientManager_, mainWindow_);

    connect(historyDialog, &PricingModelProductHistoryDialog::statusChanged,
            this, [self = QPointer<PricingModelProductController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &PricingModelProductHistoryDialog::errorOccurred,
            this, [self = QPointer<PricingModelProductController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &PricingModelProductHistoryDialog::revertVersionRequested,
            this, &PricingModelProductController::onRevertVersion);
    connect(historyDialog, &PricingModelProductHistoryDialog::openVersionRequested,
            this, &PricingModelProductController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Pricing Model Product History: %1").arg(code));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<PricingModelProductController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void PricingModelProductController::onOpenVersion(
    const analytics::domain::pricing_model_product& product, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for pricing model product: " << product.pricing_engine_type_code;

    const QString code = QString::fromStdString(product.pricing_engine_type_code);
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new PricingModelProductDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setProduct(product);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &PricingModelProductDetailDialog::statusMessage,
            this, [self = QPointer<PricingModelProductController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &PricingModelProductDetailDialog::errorMessage,
            this, [self = QPointer<PricingModelProductController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Pricing Model Product: %1 (Version %2)")
        .arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<PricingModelProductController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void PricingModelProductController::onRevertVersion(
    const analytics::domain::pricing_model_product& product) {
    BOOST_LOG_SEV(lg(), info) << "Reverting pricing model product to version: "
                              << product.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new PricingModelProductDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setProduct(product);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &PricingModelProductDetailDialog::statusMessage,
            this, &PricingModelProductController::statusMessage);
    connect(detailDialog, &PricingModelProductDetailDialog::errorMessage,
            this, &PricingModelProductController::errorMessage);
    connect(detailDialog, &PricingModelProductDetailDialog::productSaved,
            this, [self = QPointer<PricingModelProductController>(this)](const QString& code) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Pricing Model Product reverted: " << code.toStdString();
        emit self->statusMessage(QString("Pricing Model Product '%1' reverted successfully").arg(code));
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Pricing Model Product: %1")
        .arg(QString::fromStdString(product.pricing_engine_type_code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* PricingModelProductController::listWindow() const {
    return listWindow_;
}

}
