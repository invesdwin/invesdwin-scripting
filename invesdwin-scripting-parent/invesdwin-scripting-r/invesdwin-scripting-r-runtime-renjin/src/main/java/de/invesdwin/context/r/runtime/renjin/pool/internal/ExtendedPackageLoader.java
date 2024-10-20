package de.invesdwin.context.r.runtime.renjin.pool.internal;

import java.util.Optional;

import javax.annotation.concurrent.ThreadSafe;

import org.renjin.primitives.packaging.ClasspathPackageLoader;
import org.renjin.primitives.packaging.FqPackageName;
import org.renjin.primitives.packaging.Package;
import org.renjin.primitives.packaging.PackageLoader;

import de.invesdwin.context.r.runtime.renjin.RenjinProperties;
import de.invesdwin.util.collections.loadingcache.ALoadingCache;

@ThreadSafe
public final class ExtendedPackageLoader implements PackageLoader {

    public static final ExtendedPackageLoader INSTANCE = new ExtendedPackageLoader();

    private final ClasspathPackageLoader delegate = new ClasspathPackageLoader();

    private final ALoadingCache<FqPackageName, Optional<Package>> loader = new ALoadingCache<FqPackageName, Optional<Package>>() {
        @Override
        protected Optional<Package> loadValue(final FqPackageName key) {
            final Optional<Package> pkg = delegate.load(key);
            if (!pkg.isPresent()) {
                for (final String groupId : RenjinProperties.getExtendedPackageSearchGroupIds()) {
                    final FqPackageName newKey = new FqPackageName(groupId, key.getPackageName());
                    final Optional<Package> newPkg = delegate.load(newKey);
                    if (newPkg.isPresent()) {
                        return newPkg;
                    }
                }
            }
            return pkg;
        }
    };

    private ExtendedPackageLoader() {}

    @Override
    public Optional<Package> load(final FqPackageName packageName) {
        return loader.get(packageName);
    }

    @Override
    public Optional<Package> load(final String packageName) {
        return load(new FqPackageName(FqPackageName.BASE.getGroupId(), packageName));
    }

}
