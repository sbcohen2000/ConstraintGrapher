
x_axis = linspace(-10, 10, 100);
y_axis = linspace(-10, 10, 100);
z = zeros(length(y_axis), length(x_axis));

for xx = (1:length(x_axis))
  for yy = (1:length(y_axis))
    x = x_axis(xx);
    y = y_axis(yy);
    a = sqrt(x^2 + y^2) - 2;
    b = 0;
    z(yy, xx) = a^2 + b^2;
  end
end

imagesc(x_axis, y_axis, log10(z / max(max(z))));
colormap summer;
hold on;

% Draw path

raw = dlmread('data.csv', ',', 2, 0);

x = raw(:,1);
y = raw(:,2);
dx = raw(:,3);
dy = raw(:,4);

quiver(x, y, dx, dy);
hold on;

xlabel('x_0');
ylabel('x_1');
