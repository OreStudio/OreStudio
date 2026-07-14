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
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/domain/crm_topology_config.hpp"
#include <QComboBox>
#include <QDialog>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSet>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <optional>

namespace ores::qt {

using namespace ores::logging;

namespace {
const QString all_display_name = QObject::tr("All");

/// Which named CRM to open -- std::nullopt if the user cancelled.
/// A small custom dialog rather than QInputDialog::getItem() so the
/// buttons match this app's own icon convention (Icon::Checkmark /
/// Icon::Dismiss, see PartyPickerDialog) instead of the platform's
/// default OK/Cancel icons.
std::optional<QString> pick_crm_name(QWidget* parent, const QStringList& choices) {
    QDialog dialog(parent);
    dialog.setWindowTitle(QObject::tr("Cross-Rates Matrix"));

    auto* mainLayout = new QVBoxLayout(&dialog);
    mainLayout->addWidget(new QLabel(QObject::tr("Open which CRM?"), &dialog));

    auto* combo = new QComboBox(&dialog);
    combo->addItems(choices);
    mainLayout->addWidget(combo);

    auto* okButton = new QPushButton(QObject::tr("Open"), &dialog);
    auto* cancelButton = new QPushButton(QObject::tr("Cancel"), &dialog);
    okButton->setDefault(true);
    okButton->setIcon(IconUtils::createRecoloredIcon(Icon::Checkmark, IconUtils::DefaultIconColor));
    cancelButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
    QObject::connect(okButton, &QPushButton::clicked, &dialog, &QDialog::accept);
    QObject::connect(cancelButton, &QPushButton::clicked, &dialog, &QDialog::reject);

    auto* btnLayout = new QHBoxLayout();
    btnLayout->addStretch();
    btnLayout->addWidget(cancelButton);
    btnLayout->addWidget(okButton);
    mainLayout->addSpacing(4);
    mainLayout->addLayout(btnLayout);

    if (dialog.exec() != QDialog::Accepted)
        return std::nullopt;
    return combo->currentText();
}

} // namespace

CrmCrossRatesMatrixController::CrmCrossRatesMatrixController(QMainWindow* mainWindow,
                                                              QMdiArea* mdiArea,
                                                              ClientManager* clientManager,
                                                              ImageCache* imageCache,
                                                              QObject* parent)
    : QObject(parent)
    , mainWindow_(mainWindow)
    , mdiArea_(mdiArea)
    , clientManager_(clientManager)
    , imageCache_(imageCache) {

    BOOST_LOG_SEV(lg(), debug) << "CrmCrossRatesMatrixController created";
}

void CrmCrossRatesMatrixController::showMatrix() {
    if (!clientManager_)
        return;

    QPointer<CrmCrossRatesMatrixController> self = this;
    QFuture<std::expected<std::vector<refdata::domain::crm_topology_config>, QString>> future =
        QtConcurrent::run([self]() {
            if (!self || !self->clientManager_)
                return std::expected<std::vector<refdata::domain::crm_topology_config>, QString>(
                    std::vector<refdata::domain::crm_topology_config>{});
            return fetch_crm_topology_configs(self->clientManager_);
        });

    auto* watcher = new QFutureWatcher<
        std::expected<std::vector<refdata::domain::crm_topology_config>, QString>>(this);
    connect(watcher,
            &QFutureWatcher<
                std::expected<std::vector<refdata::domain::crm_topology_config>, QString>>::
                finished,
            this,
            [self, watcher]() {
                const auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;

                QSet<QString> names;
                if (result) {
                    for (const auto& config : *result)
                        names.insert(QString::fromStdString(config.name));
                }
                QStringList sorted(names.begin(), names.end());
                std::sort(sorted.begin(), sorted.end());

                QStringList choices;
                choices << all_display_name;
                choices << sorted;

                const auto choice = pick_crm_name(self->mainWindow_, choices);
                if (!choice)
                    return;

                self->openMatrix(*choice == all_display_name ? QString() : *choice);
            });
    watcher->setFuture(future);
}

void CrmCrossRatesMatrixController::openMatrix(const QString& crmName) {
    // Reuse this name's window if already open, rather than opening a
    // duplicate.
    if (auto it = matrixSubWindows_.find(crmName); it != matrixSubWindows_.end() && *it) {
        mdiArea_->setActiveSubWindow(*it);
        return;
    }

    BOOST_LOG_SEV(lg(), info)
        << "Opening CRM cross-rates matrix: " << (crmName.isEmpty() ? "All" : crmName.toStdString());

    auto* matrixWindow = new CrmCrossRatesMatrixMdiWindow(clientManager_, imageCache_, crmName);

    connect(matrixWindow,
            &CrmCrossRatesMatrixMdiWindow::statusChanged,
            this,
            [self = QPointer<CrmCrossRatesMatrixController>(this)](const QString& msg) {
                if (!self)
                    return;
                emit self->statusMessage(msg);
            });
    connect(matrixWindow,
            &CrmCrossRatesMatrixMdiWindow::errorOccurred,
            this,
            [self = QPointer<CrmCrossRatesMatrixController>(this)](const QString& err) {
                if (!self)
                    return;
                emit self->errorMessage(err);
            });

    const auto geometryKey =
        QStringLiteral("CrmCrossRatesMatrixWindow_") + (crmName.isEmpty() ? QStringLiteral("All") : crmName);

    auto* matrixSubWindow = new DetachableMdiSubWindow(mainWindow_);
    matrixSubWindow->setAttribute(Qt::WA_DeleteOnClose);
    matrixSubWindow->setWidget(matrixWindow);
    matrixSubWindow->setWindowTitle(crmName.isEmpty() ? tr("Cross-Rates Matrix — All")
                                                       : tr("Cross-Rates Matrix — %1").arg(crmName));
    matrixSubWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Currency, IconUtils::DefaultIconColor));
    matrixSubWindow->setGeometryKey(geometryKey);

    matrixSubWindows_.insert(crmName, matrixSubWindow);

    connect(matrixSubWindow,
            &QObject::destroyed,
            this,
            [self = QPointer<CrmCrossRatesMatrixController>(this), crmName, matrixSubWindow]() {
                if (!self)
                    return;
                self->matrixSubWindows_.remove(crmName);
                // MainWindow's Window menu (allDetachableWindows_) relies
                // on this to prune closed windows -- without it, every
                // closed CRM matrix window leaves a stale entry there for
                // the rest of the session (EntityController-derived
                // controllers get this for free; this one manages its own
                // windows directly, so it must emit it itself).
                emit self->detachableWindowDestroyed(matrixSubWindow);
            });

    emit detachableWindowCreated(matrixSubWindow);

    mdiArea_->addSubWindow(matrixSubWindow);
    if (!UiPersistence::restoreMdiGeometry(geometryKey, matrixSubWindow))
        matrixSubWindow->adjustSize();
    matrixSubWindow->show();
}

void CrmCrossRatesMatrixController::closeAllWindows() {
    for (const auto& subWindow : std::as_const(matrixSubWindows_)) {
        if (subWindow)
            subWindow->close();
    }
    matrixSubWindows_.clear();
}

}
