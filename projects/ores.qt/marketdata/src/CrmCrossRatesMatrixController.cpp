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
#include "ores.qt/CrmCrossRatesMatrixController.hpp"
#include "ores.qt/CrmCrossRatesMatrixMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"

namespace ores::qt {

using namespace ores::logging;

CrmCrossRatesMatrixController::CrmCrossRatesMatrixController(QMainWindow* mainWindow,
                                                              QMdiArea* mdiArea,
                                                              ClientManager* clientManager,
                                                              QObject* parent)
    : QObject(parent)
    , mainWindow_(mainWindow)
    , mdiArea_(mdiArea)
    , clientManager_(clientManager)
    , matrixWindow_(nullptr)
    , matrixSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "CrmCrossRatesMatrixController created";
}

void CrmCrossRatesMatrixController::showMatrix() {
    // Reuse existing window if still open.
    if (matrixSubWindow_) {
        mdiArea_->setActiveSubWindow(matrixSubWindow_);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Opening CRM cross-rates matrix";

    matrixWindow_ = new CrmCrossRatesMatrixMdiWindow(clientManager_);

    connect(matrixWindow_,
            &CrmCrossRatesMatrixMdiWindow::statusChanged,
            this,
            [self = QPointer<CrmCrossRatesMatrixController>(this)](const QString& msg) {
                if (!self)
                    return;
                emit self->statusMessage(msg);
            });
    connect(matrixWindow_,
            &CrmCrossRatesMatrixMdiWindow::errorOccurred,
            this,
            [self = QPointer<CrmCrossRatesMatrixController>(this)](const QString& err) {
                if (!self)
                    return;
                emit self->errorMessage(err);
            });

    matrixSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    matrixSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    matrixSubWindow_->setWidget(matrixWindow_);
    matrixSubWindow_->setWindowTitle(tr("CRM Cross-Rates Matrix"));
    matrixSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));

    connect(matrixSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<CrmCrossRatesMatrixController>(this)]() {
                if (!self)
                    return;
                self->matrixWindow_ = nullptr;
                self->matrixSubWindow_ = nullptr;
            });

    emit detachableWindowCreated(matrixSubWindow_);

    mdiArea_->addSubWindow(matrixSubWindow_);
    matrixSubWindow_->adjustSize();
    matrixSubWindow_->show();
}

void CrmCrossRatesMatrixController::closeAllWindows() {
    if (matrixSubWindow_) {
        matrixSubWindow_->close();
        matrixSubWindow_ = nullptr;
        matrixWindow_ = nullptr;
    }
}

}
