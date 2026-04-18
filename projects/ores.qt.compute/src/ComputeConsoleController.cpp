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
#include "ores.qt/ComputeConsoleController.hpp"

#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ComputeConsoleWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"

namespace ores::qt {

using namespace ores::logging;

ComputeConsoleController::ComputeConsoleController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    ChangeReasonCache* changeReasonCache,
    BadgeCache* badgeCache,
    QObject* parent)
    : QObject(parent),
      mainWindow_(mainWindow),
      mdiArea_(mdiArea),
      clientManager_(clientManager),
      changeReasonCache_(changeReasonCache),
      badgeCache_(badgeCache) {

    BOOST_LOG_SEV(lg(), debug) << "ComputeConsoleController created";
}

void ComputeConsoleController::showConsole() {
    if (subWindow_) {
        mdiArea_->setActiveSubWindow(subWindow_);
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Opening compute console";

    consoleWindow_ = new ComputeConsoleWindow(clientManager_, changeReasonCache_, badgeCache_);
    consoleWindow_->setHttpBaseUrl(http_base_url_);

    connect(consoleWindow_, &ComputeConsoleWindow::statusChanged,
            this, [self = QPointer<ComputeConsoleController>(this)](
                      const QString& msg) {
        if (!self) return;
        emit self->statusMessage(msg);
    });
    connect(consoleWindow_, &ComputeConsoleWindow::errorOccurred,
            this, [self = QPointer<ComputeConsoleController>(this)](
                      const QString& err) {
        if (!self) return;
        emit self->errorMessage(err);
    });

    subWindow_ = new DetachableMdiSubWindow(mainWindow_);
    subWindow_->setAttribute(Qt::WA_DeleteOnClose);
    subWindow_->setWidget(consoleWindow_);
    subWindow_->setWindowTitle(tr("Compute Console"));
    subWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::ServerLink, IconUtils::DefaultIconColor));

    connect(subWindow_, &QObject::destroyed, this,
            [self = QPointer<ComputeConsoleController>(this)]() {
        if (!self) return;
        self->consoleWindow_ = nullptr;
        self->subWindow_ = nullptr;
    });

    mdiArea_->addSubWindow(subWindow_);
    subWindow_->resize(consoleWindow_->sizeHint());
    subWindow_->show();

    emit detachableWindowCreated(subWindow_);
}

void ComputeConsoleController::setHttpBaseUrl(const std::string& url) {
    BOOST_LOG_SEV(lg(), info)
        << "setHttpBaseUrl url='" << (url.empty() ? "(empty)" : url)
        << "', console_window=" << (consoleWindow_ ? "present" : "null");
    http_base_url_ = url;
    if (consoleWindow_)
        consoleWindow_->setHttpBaseUrl(url);
}

void ComputeConsoleController::closeAllWindows() {
    if (subWindow_)
        subWindow_->close();
}

} // namespace ores::qt
